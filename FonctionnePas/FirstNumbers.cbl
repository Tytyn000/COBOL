       program-id. untitled.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 Nb PIC 9(6) VALUE 99.
       01 I PIC 9(6).
       01 IsFirst PIC 9 OCCURS 0 TO 99 TIMES DEPENDING ON Nb.
       linkage section.

       procedure division.
           MOVE 99 TO Nb.
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

           PERFORM VARYING Nb FROM 2 BY 1 UNTIL Nb > 99
              IF IsFirst(nb) IS EQUAL TO 1
                 DISPLAY Nb
              END-IF
           END-PERFORM.
           GOBACK.

       end program untitled.
