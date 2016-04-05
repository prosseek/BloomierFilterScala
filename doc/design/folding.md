### [2016/04/04]02:42PM

Hello:"01234" (5 bytes) is folded into two bytes.
This is the result.

    Hello2
    4:0
    Hello1
    2:3
    Hello
    0:1

`BloomierFilter/adjustByteArray` uses zeroPatch for patching.
In `(Hello2 -> 4:0)`, the `4:0` shows the patch is added high bytes
in the byte array. 