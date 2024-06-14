       program-id. Factorial.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 NbInput PIC S9(2) VALUE 0.
       01 NbOutput PIC 9(38) VALUE 1.
       01 NbOfAttempts PIC 9(2) VALUE 0.
       01 Nb PIC *B***B***B***B***B***B***B***B***B***B***B***B***.
       *>Remplacer * par Z pour avoir des espaces
       linkage section.

       procedure division.
           DISPLAY "Nombre dont vous voulez connaitre la factorielle".
           DISPLAY "Le nombre doit être un entier naturel".
           DISPLAY "Ne peut pas calculer la factorielle au dela de 33".
           ACCEPT NbInput.
           IF (NbInput < 0) THEN
              DISPLAY "La factorielle d'un négatif est impossible"
              STOP RUN
           else if (NbInput = 0) THEN
              DISPLAY "La factorielle de " NbInput " est 1"
              STOP RUN
           ELSE IF (NbInput > 33) THEN
              DISPLAY "Ne peut être calculer"
              STOP RUN
           END-IF.

           PERFORM CalculateFactorial.
           goback.
       CalculateFactorial.
           MOVE NbInput TO NbOutput
           PERFORM UNTIL NbOfAttempts = (NbInput - 1)
              ADD 1 TO NbOfAttempts
              COMPUTE NbOutput = NbOutput * NbOfAttempts
           END-PERFORM.
           MOVE NbOutput TO Nb
           DISPLAY "La factorielle de "NbInput" est " Nb.
       end program Factorial.
