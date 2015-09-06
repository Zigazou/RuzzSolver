RuzzSolver
==========

RuzzSolver is a Ruzzle/Boggle solver. Given a problem and a dictionary, it will
find every possible word contained by the problem and calculates the score.

A french dictionary is included in the dictionary directory.

As loading the dictionary is a big task, the solver accepts many problems at once
thus loading the dictionary only one time.

Problem file format
-------------------

A problem is recorded in an ASCII text file containing 4 lines of 8 characters.

A cell consists of 2 characters (thus 4×2=8 characters per line).

These 2 characters are:

- the uppercase letter from A to Z (no other character),
- the multiplier
    - d = double letter
    - t = triple letter
    - D = double word
    - T = triple word
    - any other letter = no multiplier

For example, the problem:

    UdA_U_Tt
    S_UDNTCt
    T_A_R_S_
    E_I_TDC_

has the following letters and multipliers:

    UAUT    d__t
    SUNC    _DTt
    TARS    ____
    EITC    __D_

Note: the test directory contains problem files (`test/problem-??.txt`).
    
Scores
------

Letters have values:

- 1 point: a e i l n o r s t u
- 2 points: d g m
- 3 points: b c p
- 4 points: f h v
- 8 points: j q
- 10 points: k w x y z

Multipliers change the value of the letter or of the entire word. When multiplying
a word, multipliers are multiplied. For example, a word combining a word multiplier
of 3 and a word multiplier of 2 will have a resulting word multiplier of 6.

A bonus is added at the end according to the word length:

- 5 points: 5 letters word
- 10 points: 6 letters word
- 15 points: 7 letters word
- 20 points: 8 letters word
- 25 points: 9+ letters word
