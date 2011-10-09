
clj-vorbis
==========

clj-vorbis is an easy to use ogg-vorbis decoder library.

It depends on the jogg and jorbis libraries from jcraft.

Sample usage
------------

    (use 'clj-vorbis.core)
    (import 'java.nio.channels.WritableByteChannel)
    (import 'java.nio.ByteBuffer)
    (let [ds (init-vorbis (java.io.FileInputStream. "path_to_an_ogg_vorbis_file.ogg"))
          ^WritableByteChannel c (.getChannel (java.io.FileOutputStream. "decoded_vorbis_data.raw"))
          ^ByteBuffer buf (.order (ByteBuffer/allocate 8192) (java.nio.ByteOrder/LITTLE_ENDIAN))]
      (try
        (loop [bytes (long (read-pcm ds buf))]
          (when (> bytes -1)
            (while (> (.remaining buf) 0)
              (.write c buf))
            (.clear buf)
            (recur (read-pcm ds buf))))
        (catch Exception e (.printStackTrace e)))
      (close-vorbis ds)
      (.close c))

