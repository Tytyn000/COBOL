       program-id. Pangram.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 Phrase PIC X(180).
       01 WordsList PIC X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
       01 I PIC 99.
       01 J PIC 9(3).
       01 SearchLetter PIC X(1).
       01 CurrentLetter PIC X(1).
       linkage section.

       procedure division.
           ACCEPT Phrase
           PERFORM ChekIfIsAPangram.
           goback.
       ChekIfIsAPangram.
           MOVE FUNCTION LOWER-CASE (Phrase) TO Phrase.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 26
              MOVE WordsList(I:I) TO SearchLetter
              MOVE 0 TO J
              PERFORM VARYING J FROM 1 BY 1 UNTIL J > 180
                 MOVE Phrase(J:J) TO CurrentLetter
                 IF (CurrentLetter = SearchLetter) THEN
                    DISPLAY CurrentLetter
                 END-IF
               END-PERFORM
           END-PERFORM.
       end program Pangram.
