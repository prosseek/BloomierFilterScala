### [2016/04/01]02:54PM

The input value of ByteArrayBloomierFilter is
[String, Array[Byte]], and the byte array can be null.
This is to enable the Complete Bloomier Filter.

CBF is implemented with some value of the input key is null.
With this configuration, we reserver the location of the table that matches
the key, and the value is null (not assigned).
