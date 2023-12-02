       program-id. untitled.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 Nb PIC 9(21) VALUE 9999.
       01 I PIC 9(21).
       01 IsFirst PIC 9 OCCURS 0 TO 9999 TIMES DEPENDING ON Nb.
       linkage section.

       procedure division.
           MOVE 1 TO Nb
           PERFORM VARYING Nb FROM 2 BY 1 UNTIL Nb > 99
              MOVE 1 TO IsFirst(Nb)
           END-PERFORM.

           PERFORM VARYING Nb FROM 2 BY 1 UNTIL Nb * Nb > 99
              IF IsFirst(Nb) IS EQUAL TO 1
                 PERFORM VARYING I FROM Nb BY Nb UNTIL I > 99
                    MOVE 0 TO IsFirst(I)
                 END-PERFORM
              END-IF
           END-PERFORM.

           DISPLAY "nb premiers".
           PERFORM VARYING Nb FROM 2 BY 1 UNTIL Nb > 99
              IF IsFirst(nb) IS EQUAL TO 1
                 DISPLAY Nb
              END-IF
           END-PERFORM.
           GOBACK.

       end program untitled.
