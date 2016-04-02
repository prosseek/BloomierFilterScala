### [2016/04/01]05:39PM

The information needed for storing "ByteArrayBloomierFilter"

#### Header (2 bytes)
* m (0 - 255) => (8 unsigned bits)
* Q (1,2,4,8 only) => 2 bits
* hashValue (0 - 63) => 6 bits

#### Table
* table: m/8 + Q*n
    * m/8 - location bits
    * Q*n - byte arrays

#### Things to consider

The parameters we don't need to store
* k = 3 (always use 3, so we don't store k)

We can reduce the headers size with

1. m = 255 (max)
2. Q = 2

With this configuration, we need only

* hashValue to be stored (1 byte)