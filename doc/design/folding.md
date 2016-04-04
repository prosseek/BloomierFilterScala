### [2016/04/04]02:42PM

Hello:"01234" (5 bytes) is folded into two bytes.
This is the result.

    Hello2
    0:4
    Hello1
    2:3
    Hello
    0:1

`BloomierFilter/adjustByteArray` uses adjust for patching.
In `(Hello2 -> 0:4)`, the `0:4` shows the patch is added low bytes
in the byte array. 