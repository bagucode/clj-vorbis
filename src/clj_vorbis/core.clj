(ns clj-vorbis.core
  (:import [com.jcraft.jogg Packet Page StreamState SyncState]
           [com.jcraft.jorbis DspState Block Comment Info]
           [java.io InputStream]
           [java.nio ByteBuffer]))

;; Interface to force primitive args in the deftype implementation.
;; Is there an idiomatic way to do this?
(definterface IDecoderState
  (^bytes set_buffer_BANG_ [^bytes buffer])
  (^bytes get_buffer [])
  (^long set_samples_BANG_ [^long samples])
  (^long get_samples [])
  (^long set_sample_index_BANG_ [^long idx])
  (^long get_sample_index [])
  (^ints set_pcm_index_BANG_ [^ints idxArr])
  (^ints get_pcm_index []))

(deftype DecoderState
    [^:unsynchronized-mutable ^long samples
     ^:unsynchronized-mutable ^long sample-index
     ;; The non primitive type hints don't actually work
     ;; but at least they serve as documentation...
     ^:unsynchronized-mutable ^bytes buffer
     ^:unsynchronized-mutable ^ints pcm-index
     ^"[[[F" pcm-info
     ^Packet packet
     ^Page page
     ^StreamState stream-state
     ^SyncState sync-state
     ^DspState dsp-state
     ^Block block
     ^Comment comment
     ^Info info
     ^InputStream is]
  IDecoderState
  (set-buffer! [this b]
    (set! buffer b))
  (get-buffer [this]
    buffer)
  (set-samples! [this s]
    (set! samples s))
  (get-samples [this]
    samples)
  (set-sample-index! [this idx]
    (set! sample-index idx))
  (get-sample-index [this]
    sample-index)
  (set-pcm-index! [this idx]
    (set! pcm-index idx))
  (get-pcm-index [this]
    pcm-index))

(binding [*unchecked-math* true]

  (def ^:const ^:private ^long buffer-size 8192)

  (defn- create-state
    "Creates a DecoderState instance and sets default values for it's fields"
    ^DecoderState [^InputStream is]
    (let [dsp (DspState.)]
      (DecoderState. 0                           ;; samples
                     0                           ;; sample-index
                     nil                         ;; buffer
                     nil                         ;; pcm-index
                     (make-array Float/TYPE 1 0 0) ;; pcm-info
                     (Packet.)                     ;; packet
                     (Page.)                       ;; page
                     (StreamState.)                ;; stream-state
                     (SyncState.)                  ;; sync-state
                     dsp                           ;; dsp-state
                     (Block. dsp)                  ;; block
                     (Comment.)                    ;; comment
                     (Info.)                       ;; info
                     is                            ;; is
                     )))

  (defn- read-data
    "Reads more data from the InputStream into the SyncState buffer.
  Returns number of bytes read or -1 on end of stream."
    ^long [^DecoderState ds]
    (let [bs buffer-size
          ^SyncState ss (.sync-state ds)
          idx (.buffer ss bs)
          buf (.data ss)
          ^InputStream is (.is ds)
          count (.read is buf idx bs)]
      (if (== -1 count)
        -1
        (do
          (.set-buffer! ds buf)
          (.wrote ss count)
          1))))

  (defn- next-page
    "Reads the next page of ogg data.
  Returns 1 on success or -1 on end of stream.
  If hole-is-error? is not nil or false and a hole
  in the data stream is detected, a RuntimeException is thrown."
    ^long [^DecoderState ds hole-is-error?]
    (let [bs buffer-size
          ^SyncState ss (.sync-state ds)
          ^Page page (.page ds)
          ^StreamState stream-state (.stream-state ds)]
      (condp == (.pageout ss page)
        -1 (if hole-is-error?
             (throw (RuntimeException. "Hole in data"))
             (if (== -1 (read-data ds))
               -1
               (recur ds hole-is-error?)))
        0 (if (== -1 (read-data ds))
            -1
            (recur ds hole-is-error?))
        1 (do
            (.pagein stream-state page)
            1))))

  (defn- next-packet
    "Reads the next packet of vorbis data from the current page.
  Returns 1 on success or -1 on end of stream.
  If hole-is-error? is not nil or false and a hole
  in the data stream is detected, a RuntimeException is thrown."
    ^long [^DecoderState ds hole-is-error?]
    (let [^StreamState stream-state (.stream-state ds)
          ^Page page (.page ds)
          ^Packet packet (.packet ds)]
      (condp == (.packetout stream-state packet)
        -1 (if hole-is-error?
             (throw (RuntimeException. "Hole in data"))
             (if (== -1 (next-page ds hole-is-error?))
               -1
               (recur ds hole-is-error?)))
        0 (if (== -1 (next-page ds hole-is-error?))
            -1
            (recur ds hole-is-error?))
        1 1)))

  (defn- read-header
    "Read the header of the vorbis data. This must be called
  before any calls to read-samples.
  The header includes the channel, frequency and any author comments.
  If there is an IO error or the data in the InputStream does
  not seem to be vorbis data, a RuntimeException is thrown."
    ^long [^DecoderState s]
    (let [^StreamState stream-state (.stream-state s)
          ^Info info (.info s)
          ^Comment comment (.comment s)
          ^Page page (.page s)
          ^Packet packet (.packet s)
          ^SyncState ss (.sync-state s)]
      ;; read first page "manually" because stream state needs
      ;; init before the call to pagein
      (read-data s)
      (.pageout ss page)
      (.init stream-state (.serialno page))
      (.reset stream-state)
      (.init info)
      (.init comment)
      (when (== -1 (.pagein stream-state page))
        (throw (RuntimeException. "Error when reading header page")))
      (dotimes [i 3]
        (if-not (== 1 (next-packet s true))
          (throw (RuntimeException. "Error reading header packet"))
          (let [result (.synthesis_headerin info comment packet)]
            (when (< result 0)
              (throw (RuntimeException. "Not vorbis data"))))))
      1))

  (defn- read-samples
    "Decodes vorbis data into raw pcm data and stores
  it in pcm-info. Returns the number of samples decoded
  or -1 on end of stream.
  Will ignore any data holes in the vorbis stream."
    ^long [^DecoderState ds]
    (let [^Block block (.block ds)
          ^DspState dsp (.dsp-state ds)
          ^Packet packet (.packet ds)
          ^"[[[F" pcm-info (.pcm-info ds)
          ^"[I" pcm-index (.get-pcm-index ds)
          remaining (.get-samples ds)]
      (if (> remaining 0)
        0
        (if (== -1 (next-packet ds false))
          -1
          (if-not (== 0 (.synthesis block packet))
            (recur ds) ;; not an audio packet
            (do
              (.synthesis_blockin dsp block)
              (let [samples (long (.synthesis_pcmout dsp pcm-info pcm-index))]
                (if (== 0 samples)
                  (recur ds) ;; need more data
                  (do
                    (.set-samples! ds samples)
                    (.set-sample-index! ds 0)
                    samples)))))))))

  ;; Public

  (defn init-vorbis
    "Initializes a decoding context for vorbis data and
  reads the vorbis header from the given stream. If the
  stream cannot be read, ends prematurely or is not
  vorbis data, a RuntimeException will be thrown.
  Return value is a decoder object which is used to read
  pcm data from the stream. Note that the decoder object
  is full of mutable data and as such is not thread safe.
  Do not call any vorbis functions from different threads
  using the same decoder state."
    ^DecoderState [^InputStream is]
    (let [^DecoderState state (create-state is)
          ^SyncState ss (.sync-state state)
          bs buffer-size
          ^DspState dsp (.dsp-state state)
          ^Block block (.block state)
          ^Info info (.info state)]
      (.init ss)
      (.buffer ss bs)
      (.set-buffer! state (.data ss))
      (read-header state)
      (.synthesis_init dsp info)
      (.init block dsp)
      (.set-pcm-index! state (int-array (.channels info)))
      state))

  (defn decoder-state?
    "Returns true if obj is a vorbis decoder state"
    [obj]
    (instance? DecoderState obj))

  (defn close-vorbis
    "Cleans up vorbis decoding resources.
  If close-stream? is true, also closes the associated
  InputStream object."
    ([^DecoderState ds]
       (close-vorbis ds false))
    ([^DecoderState ds close-stream?]
       (let [^StreamState ss (.stream-state ds)
             ^Block block (.block ds)
             ^DspState dsp (.dsp-state ds)
             ^Info info (.info ds)
             ^SyncState syncs (.sync-state ds)
             ^InputStream is (.is ds)]
         (.clear ss)
         (.clear block)
         (.clear dsp)
         (.clear info)
         (.clear syncs)
         (when close-stream?
           (.close is)))
       nil))

  (defn channels
    "Returns number of channels in a vorbis stream."
    ^long [^DecoderState ds]
    (.channels ^Info (.info ds)))

  (defn hz
    "Returns the sample frequency of a vorbis stream."
    ^long [^DecoderState ds]
    (.rate ^Info (.info ds)))

  (defn read-pcm
    "Tries to fill the supplied buffer with pcm data.
  Returns number of bytes written or -1 if
  end of stream was reached.
  The limit of the buffer will be set after the last
  sample written and the position will not be altered
  which means any relative get operations will work
  without calling flip on the buffer.
  Samples will be written as 16 bit signed integer
  values in the byte order used by the supplied buffer."
    ^long [^DecoderState ds ^ByteBuffer buf]
    (let [channels (channels ds)
          max-samples (unchecked-divide-int (.remaining buf) (* 2 channels))
          ^DspState dsp (.dsp-state ds)]
      (if (== 0 (.get-samples ds))
        (if (== -1 (read-samples ds))
          -1
          (recur ds buf))
        (let [samples (.get-samples ds)
              sample-index (.get-sample-index ds)
              range (min max-samples (- samples sample-index))
              ^"[[F" pcm-info0 (aget ^"[[[F" (.pcm-info ds) 0)
              ^"[I" pcm-index (.get-pcm-index ds)]
          (dotimes [c channels]
            (dotimes [s range]
              (let [^"[F" pcm-info (aget pcm-info0 c)
                    ;; incorporate sample index here again?
                    ;; if the given buffer is small enough then
                    ;; it will be needed...
                    lval (long (* 32767 (aget pcm-info
                                              ;; (+ sample-index)
                                              (+ s (aget pcm-index c)))))
                    sval (short (if (> lval 32767)
                                  32767
                                  (if (< lval -32768)
                                    -32768
                                    (if (< lval 0)
                                      (bit-or lval 32768)
                                      lval))))
                    idx (int (+ (* 2 c) (* 2 (* s channels))))]
                (.putShort buf idx sval))))
          (.synthesis_read dsp range)
          (.set-samples! ds (- samples range))
          (.set-sample-index! ds (+ sample-index range))
          (let [written (* range (* 2 channels))]
            (.limit buf (int (+ written (.position buf))))
            written)))))

  ) ;; binding *unchecked-math* true
