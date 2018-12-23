## Example of expression

`ADD R3,R1,R2`

## Format of response

> \<type>[\<number of bits>]

`OP[4] R1[3] R2[3] R3[3] 0[3]`


## Process

> OP * 2^(16-4)

> R1 * 2^(16-4-3)

> R2 * 2^(16-4-3-3)

> R3 * 2^(16-4-3-3-3)

## Example of calculation process

> R3 = 010 (defined randomly, by example)

ADD R3,5,6

> 0001 is the code of ADD operation on refference file

ADD -> 0001 * 2^(12) -> 0001000000000000 <br>
R1  -> 101  * 2^(9)  -> 0000101000000000 <br>
R2  -> 110  * 2^(6)  -> 0000000110000000 <br>
R3  -> 010  * 2^(3)  -> 0000000000010000 <br>


> Results:

0001000000000000 +<br>
0000101000000000 +<br>
0000000110000000 +<br>
0000000000010000 <br>
\----------------------- <br>
0001101110010000

> Convert to Hexadecimal

0001101110010000 <br>
0001 -> 1 <br>
1011 -> B <br>
1001 -> 9 <br>
0000 -> 0 <br>

Result: **1B90**