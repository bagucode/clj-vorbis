(ns clj-vorbis.core
  (:import [com.jcraft.jogg Packet Page StreamState SyncState]
           [com.jcraft.jorbis DspState Block Comment Info]
           [java.io InputStream]
           [java.nio ByteBuffer]))

(def ^{:private true :tag Integer}
  buffer-size 8192)

(deftype DecoderState
    [buffer
     integers
     pcmInfo
     pcmIndex
     packet
     page
     streamState
     syncState
     dspState
     block
     comment
     info
     is
     ])

(defn- create-state ^DecoderState
  [^InputStream is]
  (let [dsp (DspState.)]
    (DecoderState. (make-array Byte/TYPE 1 0)
                   (make-array Integer/TYPE 2)
                   (make-array Float/TYPE 1 0 0)
                   (make-array Integer/TYPE 1 0)
                   (Packet.) (Page.) (StreamState.)
                   (SyncState.) dsp (Block. dsp)
                   (Comment.) (Info.) is)))

(defn- set-buffer!
  [^DecoderState s ^"[B" b]
  (let [^"[[B" container (.buffer s)]
    (aset container 0 b)))

(defn- get-buffer ^"[B"
  [^DecoderState s]
  (aget ^"[[B" (.buffer s) 0))

(defn- set-samples!
  [^DecoderState ds ^long samples]
  (let [^"[I" integers (.integers ds)]
    (aset-int integers 0 samples)))

(defn- get-samples ^long
  [^DecoderState ds]
  (aget ^"[I" (.integers ds) 0))

(defn- set-sample-index!
  [^DecoderState ds ^long sample-index]
  (let [^"[I" integers (.integers ds)]
    (aset-int integers 1 sample-index)))

(defn- get-sample-index ^long
  [^DecoderState ds]
  (aget ^"[I" (.integers ds) 1))

(defn- set-pcm-index!
  [^DecoderState ds ^"[I" array]
  (let [^"[[I" container (.pcmIndex ds)]
    (aset container 0 array)))

(defn- get-pcm-index
  [^DecoderState ds]
  (aget ^"[[I" (.pcmIndex ds) 0))

(defn- read-data ^long
  [^DecoderState ds]
  (let [bs (int buffer-size)
        ^SyncState ss (.syncState ds)
        idx (int (.buffer ss bs))
        buf (.data ss)
        ^InputStream is (.is ds)
        count (.read is buf idx bs)]
    (if (= -1 count)
      -1
      (do
        (set-buffer! ds buf)
        (.wrote ss count)
        1))))

(defn- next-page ^long
  [^DecoderState ds hole-is-error?]
  (let [bs (int buffer-size)
        ^SyncState ss (.syncState ds)
        ^Page page (.page ds)
        ^StreamState stream-state (.streamState ds)]
    (condp = (.pageout ss page)
      -1 (if hole-is-error?
           (throw (Exception. "Hole in data"))
           (if (= -1 (read-data ds))
             -1
             (recur ds hole-is-error?)))
      0 (if (= -1 (read-data ds))
          -1
          (recur ds hole-is-error?))
      1 (do
          (.pagein stream-state page)
          1))))

(defn- next-packet ^long
  [^DecoderState ds hole-is-error?]
  (let [^StreamState stream-state (.streamState ds)
        ^Page page (.page ds)
        ^Packet packet (.packet ds)]
    (condp = (.packetout stream-state packet)
      -1 (if hole-is-error?
           (throw (Exception. "Hole in data"))
           (if (= -1 (next-page ds hole-is-error?))
             -1
             (recur ds hole-is-error?)))
      0 (if (= -1 (next-page ds hole-is-error?))
          -1
          (recur ds hole-is-error?))
      1 1)))

(defn- read-header ^long
  [^DecoderState s]
  (let [^StreamState stream-state (.streamState s)
        ^Info info (.info s)
        ^Comment comment (.comment s)
        ^Page page (.page s)
        ^Packet packet (.packet s)
        ^SyncState ss (.syncState s)]
    ;; read first page "manually" because stream state needs
    ;; init before the call to pagein
    (read-data s)
    (.pageout ss page)
    (.init stream-state (.serialno page))
    (.reset stream-state)
    (.init info)
    (.init comment)
    (when (= -1 (.pagein stream-state page))
      (throw (Exception. "Error when reading header page")))
    (dotimes [i 3]
      (if (not= 1 (next-packet s true))
        (throw (Exception. "Error reading header packet"))
        (let [result (.synthesis_headerin info comment packet)]
          (when (< result 0)
            (throw (Exception. "Not vorbis data"))))))
    1))

(defn- read-samples ^long
  [^DecoderState ds]
  (let [^Block block (.block ds)
        ^DspState dsp (.dspState ds)
        ^Packet packet (.packet ds)
        ^"[[[F" pcm-info (.pcmInfo ds)
        ^"[I" pcm-index (get-pcm-index ds)
        remaining (get-samples ds)]
    (if (> remaining 0)
      0
      (if (= -1 (next-packet ds false))
        -1
        (if (not= 0 (.synthesis block packet))
          (recur ds) ;; not an audio packet
          (do
            (.synthesis_blockin dsp block)
            (let [samples (long (.synthesis_pcmout dsp pcm-info pcm-index))]
              (if (= 0 samples)
                (recur ds) ;; need more data
                (do
                  (set-samples! ds samples)
                  (set-sample-index! ds 0)
                  samples)))))))))

;; Public

(defn init-vorbis
  "Initializes a decoding context for vorbis data and
 reads the vorbis header from the given stream. If the
 stream cannot be read, ends prematurely or is not
 vorbis data, an exception will be thrown.
 Return value is a decoder object which is used to read
 pcm data from the stream. Note that the decoder object
 is full of mutable data and as such is not thread safe.
 Do not call any vorbis functions from different threads
 using the same decoder state."
  {:tag DecoderState}
  [^InputStream is]
  (let [^DecoderState state (create-state is)
        ^SyncState ss (.syncState state)
        bs (int buffer-size)
        ^DspState dsp (.dspState state)
        ^Block block (.block state)
        ^Info info (.info state)]
    (.init ss)
    (.buffer ss bs)
    (set-buffer! state (.data ss))
    (read-header state)
    (.synthesis_init dsp info)
    (.init block dsp)
    (set-pcm-index! state (int-array (.channels info)))
    state))

(defn close-vorbis
  "Cleans up vorbis decoding resources.
 If close-stream? is true, also closes the associated
 InputStream object."
  ([^DecoderState ds]
     (close-vorbis ds false))
  ([^DecoderState ds close-stream?]
     (let [^StreamState ss (.streamState ds)
           ^Block block (.block ds)
           ^DspState dsp (.dspState ds)
           ^Info info (.info ds)
           ^SyncState syncs (.syncState ds)
           ^InputStream is (.is ds)]
       (.clear ss)
       (.clear block)
       (.clear dsp)
       (.clear info)
       (.clear syncs)
       (when close-stream?
         (.close is)))
     nil))

(defn channels?
  "Returns number of channels in a vorbis stream."
  [^DecoderState ds]
  (.channels ^Info (.info ds)))

(defn hz?
  "Returns the sample frequency of a vorbis stream."
  [^DecoderState ds]
  (.rate ^Info (.info ds)))

(defn read-pcm
  "Tries to fill the supplied buffer with pcm data.
 Returns number of bytes written or -1 if
 end of stream was reached.
 The limit of the buffer will be set after the last
 sample written and the position will not be altered
 which means any relative get operations will work
 without calling flip on the buffer
 The byte order of the buffer must be little endian."
  ^long [^DecoderState ds ^ByteBuffer buf]
  (let [^Info info (.info ds)
        channels (.channels info)
        max-samples (unchecked-divide-int (.remaining buf) (* 2 channels))
        ^DspState dsp (.dspState ds)]
    (if (= 0 (get-samples ds))
      (if (= -1 (read-samples ds))
        -1
        (recur ds buf))
      (let [samples (get-samples ds)
            sample-index (get-sample-index ds)
            range (min max-samples (- samples sample-index))
            ^"[[F" pcm-info0 (aget ^"[[[F" (.pcmInfo ds) 0)
            ^"[I" pcm-index (get-pcm-index ds)]
        (dotimes [c channels]
          (dotimes [s range]
            (let [^"[F" pcm-info (aget pcm-info0 c)
                  ;; incorporate sample index here again?
                  ;; if the given buffer is small enough then
                  ;; it will be needed...
                  val (int (* 32767 (aget pcm-info
                                          ;; (+ sample-index)
                                          (+ s (aget pcm-index c)))))
                  val (int (if (> val 32767)
                             32767
                             val))
                  val (int (if (< val -32768)
                             -32768
                             val))
                  val (short (if (< val 0)
                               (bit-or val 32768)
                               val))
                  idx (int (+ (* 2 c) (* 2 (* s channels))))]
              (.putShort buf idx val))))
        (.synthesis_read dsp range)
        (set-samples! ds (- samples range))
        (set-sample-index! ds (+ sample-index range))
        (let [written (* range (* 2 channels))]
          (.limit buf (int (+ written (.position buf))))
          written)))))

