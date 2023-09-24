       program-id. RandomGenerator.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 SEED.
           05 YEAR PIC 9999.
           05 MONTH PIC 99.
           05 DAYS PIC 99.
           05 HOURS PIC 99.
           05 MINUTES PIC 99.
           05 SECONDS PIC 99.
           05 HundrethsOfSeconds PIC 99.
       01 RESULT PIC 9(21).
       01 FinalResult PIC 9(20).
       01 NbOfLetters PIC 9(15).
       01 NbOfLettersCreated PIC 9(15) VALUE 0.
       linkage section.

       procedure division.
           MOVE FUNCTION CURRENT-DATE TO SEED.
           MOVE YEAR OF SEED TO RESULT(1:4)
           MOVE MONTH OF SEED TO RESULT(5:6)
           MOVE DAYS OF SEED TO RESULT(7:8)
           MOVE HOURS OF SEED TO RESULT(9:10)
           MOVE MINUTES OF SEED TO RESULT(11:12)
           MOVE SECONDS OF SEED TO RESULT(13:14)
           MOVE HundrethsOfSeconds OF SEED TO RESULT(15:16)
           DISPLAY "Nombre a générer Max 8"
           DISPLAY SEED
           ACCEPT NbOfLetters
           IF NbOfLetters <= 0 THEN
               DISPLAY "Nombre trop petit"
               STOP RUN
           END-IF
           IF NbOfLetters > 8 THEN
               DISPLAY "Nombre invalide"
               STOP RUN
           END-IF
           *>Premier caractères = millénaires
           IF RESULT (1:1) = "2" THEN
               MOVE "!" TO FinalResult (1:1)
               ADD 1 TO NbOfLettersCreated
           ELSE 
               MOVE "$" TO FinalResult (1:1)
               ADD 1 TO NbOfLettersCreated
           END-IF
           DISPLAY FinalResult
           *>Deuxième caractères = dizaine de jour
           IF (NbOfLettersCreated >= NbOfLetters)
               STOP RUN
           END-IF
           IF RESULT (7:7) = "0" THEN
               MOVE "k" TO FinalResult (2:2)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (7:7) = "1" THEN
               MOVE "*" TO FinalResult (2:2)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (7:7) = "2" THEN
               MOVE ">" TO FinalResult (2:2)
               ADD 1 TO NbOfLettersCreated
               STOP RUN
           ELSE IF RESULT (7:7) = "3" THEN
               MOVE "Z" TO FinalResult (2:2)
               ADD 1 TO NbOfLettersCreated
           ELSE 
               MOVE "?" TO FinalResult (2:2)
               ADD 1 TO NbOfLettersCreated
           END-IF
           DISPLAY FinalResult
           *>Troisième caractères = unités de centième de secondes
           IF (NbOfLettersCreated >= NbOfLetters)
               STOP RUN
           END-IF
           IF RESULT (16:16) = "0" THEN
               MOVE 'K' TO FinalResult (3:3)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (16:16) = "1" THEN
               MOVE "P" TO FinalResult (3:3)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (16:16) = "2" THEN
               MOVE "<" TO FinalResult (3:3)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (16:16) = "3" THEN
               MOVE "H" TO FinalResult (3:3)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (16:16) = "4" THEN
               MOVE "$" TO FinalResult (3:3)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (16:16) = "5" THEN
               MOVE "}" TO FinalResult (3:3)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (16:16) = "6" THEN
               MOVE "\" TO FinalResult (3:3)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (16:16) = "7" THEN
               MOVE "^" TO FinalResult (3:3)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (16:16) = "8" THEN 
               MOVE "5" TO FinalResult (3:3)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (16:16) = "9" THEN
               MOVE "#" TO FinalResult (3:3)
               ADD 1 TO NbOfLettersCreated
           ELSE
               MOVE "D" TO FinalResult (3:3)
               ADD 1 TO NbOfLettersCreated
           END-IF
           DISPLAY FinalResult
           *>Quatrième caractères = unitée des minutes
           IF (NbOfLettersCreated >= NbOfLetters)
               STOP RUN
           END-IF
           IF RESULT (12:12) = "0" THEN
               MOVE "-" TO FinalResult (4:4)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (12:12) = "1" THEN
               MOVE "P" TO FinalResult (4:4)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (12:12) = "2" THEN
               MOVE "|" TO FinalResult (4:4)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (12:12) = "3" THEN
               MOVE "," TO FinalResult (4:4)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (12:12) = "4" THEN
               MOVE "{" TO FinalResult (4:4)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (12:12) = "5" THEN
               MOVE "(" TO FinalResult (4:4)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (12:12) = "6" THEN
               MOVE "^" TO FinalResult (4:4)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (12:12) = "7" THEN
               MOVE "@" TO FinalResult (4:4)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (12:12) = "8" THEN
               MOVE "F" TO FinalResult (4:4)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (12:12) = "9" THEN
               MOVE "~" TO FinalResult (4:4)
               ADD 1 TO NbOfLettersCreated
           ELSE 
               MOVE "X" TO FinalResult (4:4)
               ADD 1 TO NbOfLettersCreated
           END-IF
           DISPLAY FinalResult
           *>Cinquième résultat = unités de jour
           IF (NbOfLettersCreated >= NbOfLetters)
               STOP RUN
           END-IF
           IF RESULT (8:8) = "0" THEN
               MOVE "d" TO FinalResult (5:5)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (8:8) = "1" THEN
               MOVE "a" TO FinalResult (5:5)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (8:8) = "2" THEN
               MOVE "j" TO FinalResult (5:5)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (8:8) = "3" THEN
               MOVE "i" TO FinalResult (5:5)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (8:8) = "4" THEN
               MOVE "9" TO FinalResult (5:5)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (8:8) = "5" THEN
               MOVE "_" TO FinalResult (5:5)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (8:8) = "6" THEN
               MOVE "c" TO FinalResult (5:5)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (8:8) = "7" THEN
               MOVE "è" TO FinalResult (5:5)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (8:8) = "8" THEN
               MOVE "9" TO FinalResult (5:5)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (8:8) = "9" THEN
               MOVE "t" TO FinalResult (5:5)
               ADD 1 TO NbOfLettersCreated
           ELSE 
               MOVE "u" TO FinalResult (5:5)
               ADD 1 TO NbOfLettersCreated
           END-IF.
           DISPLAY FinalResult
           *>Sixième caractères = unités de minutes
           IF (NbOfLettersCreated >= NbOfLetters)
               STOP RUN
           END-IF
           IF RESULT (14:14) = "0" THEN
               MOVE "q" TO FinalResult (6:6)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (14:14) = "1" THEN
               MOVE "w" TO FinalResult (6:6)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (14:14) = "2" THEN
               MOVE "e" TO FinalResult (6:6)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (14:14) = "3" THEN
               MOVE "r" TO FinalResult (6:6)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (14:14) = "4" THEN
               MOVE "t" TO FinalResult (6:6)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (14:14) = "5" THEN
               MOVE "y" TO FinalResult (6:6)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (14:14) = "6" THEN
               MOVE "u" TO FinalResult (6:6)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (14:14) = "7" THEN
               MOVE "i" TO FinalResult (6:6)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (14:14) = "8" THEN
               MOVE "o" TO FinalResult (6:6)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (14:14) = "9" THEN
               MOVE "p" TO FinalResult (6:6)
               ADD 1 TO NbOfLettersCreated
           ELSE 
               MOVE "Q" TO FinalResult (6:6)
               ADD 1 TO NbOfLettersCreated
           END-IF
           DISPLAY FinalResult
           *>septième caractères = unité des mois
           IF (NbOfLettersCreated >= NbOfLetters)
               STOP RUN
           END-IF
           IF RESULT (5:5) = "0" THEN 
               MOVE "h" TO FinalResult (7:7)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (5:5) = "1" THEN
               MOVE "b" TO FinalResult (7:7)
               ADD 1 TO NbOfLettersCreated
           ELSE 
               MOVE "J" TO FinalResult (7:7)
               ADD 1 TO NbOfLettersCreated
           END-IF
           DISPLAY FinalResult
           *>Huitième caractères = dizaine de l'heure
           IF RESULT (9:9) = "0" THEN
               MOVE "L" TO FinalResult (8:8)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (9:9) = "1" THEN
               MOVE "S" TO FinalResult (8:8)
               ADD 1 TO NbOfLettersCreated
           ELSE IF RESULT (9:9) = "2" THEN
               MOVE "Z" TO FinalResult (8:8)
               ADD 1 TO NbOfLettersCreated
           ELSE 
               MOVE "V" TO FinalResult (8:8)
               ADD 1 TO NbOfLettersCreated
           END-IF
           DISPLAY FinalResult
       DISPLAY "Calcul en cours"
       goback.
       end program RandomGenerator.
