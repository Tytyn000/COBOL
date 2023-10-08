       program-id. RandomGenerator.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 Seed PIC 9(10).
       01 Resulat PIC X(10) VALUE NULL.
       01 NbOfLetters PIC 9(10).
       01 NbOfLettersCreated PIC 9(10).
       linkage section.

       procedure division.
       *> (1:2) le 1 est la position du chiffres dans la variable
           DISPLAY "Entrez une Seed à 10 chiffres ex : 0123456789"
           DISPLAY "Une Seed non complète sera aussi prise en compte"
           ACCEPT SEED
           DISPLAY "La Seed actuelle est : "Seed
           DISPLAY "Nombre de caractère à créer max 10"
           ACCEPT NbOfLetters
           IF NbOfLetters > 10 
               DISPLAY "Chiffre trop haut"
               STOP RUN
           ELSE IF NbOfLetters <= 0 THEN
               DISPLAY "Chiffre trop bas"
               STOP RUN
           END-IF.
           DISPLAY "Génération en cours"
           IF Seed (1:1) = "0" THEN
               MOVE "p" TO Resulat (1:1)
           ELSE IF SEED (1:1) = "1" THEN
               MOVE "7" TO Resulat (1:1)
           ELSE IF SEED (1:1) = "2" THEN
               MOVE "J" TO Resulat (1:1)
           ELSE IF SEED (1:1) = "3" THEN
               MOVE "X" TO Resulat (1:1)
           ELSE IF SEED (1:1) = "4" THEN
               MOVE "v" TO Resulat (1:1)
           ELSE IF SEED (1:1) = "5" THEN
               MOVE "G" TO Resulat (1:1)
           ELSE IF SEED (1:1) = "6" THEN
               MOVE "K" TO Resulat (1:1)
           ELSE IF SEED (1:1) = "7" THEN
               MOVE "W" TO Resulat (1:1)
           ELSE IF SEED (1:1) = "8" THEN
               MOVE "5" TO Resulat (1:1)
           ELSE IF SEED (1:1) = "9" THEN
               MOVE "r" TO Resulat (1:1)
           ELSE 
               MOVE "?" TO Resulat (1:1)
           END-IF.
           ADD 1 TO NbOfLettersCreated
           IF NbOfLettersCreated >= NbOfLetters
               DISPLAY "Calcul terminé : " Resulat
               STOP RUN
           END-IF.
           IF Seed (2:1) = "0" THEN
               MOVE "M" TO Resulat (2:1)
           ELSE IF SEED (2:1) = "1" THEN
               MOVE "U" TO Resulat (2:1)
               DISPLAY Resulat
           ELSE IF SEED (2:1) = "2" THEN
               MOVE "Z" TO Resulat (2:1)
           ELSE IF SEED (2:1) = "3" THEN
               MOVE "2" TO Resulat (2:1)
           ELSE IF SEED (2:1) = "4" THEN
               MOVE "c" TO Resulat (2:1)
           ELSE IF SEED (2:1) = "5" THEN
               MOVE "1" TO Resulat (2:1)
           ELSE IF SEED (2:1) = "6" THEN
               MOVE "h" TO Resulat (2:1)
           ELSE IF SEED (2:1) = "7" THEN
               MOVE "i" TO Resulat (2:1)
           ELSE IF SEED (2:1) = "8" THEN
               MOVE "F" TO Resulat (2:1)
           ELSE IF SEED (2:1) = "9" THEN
               MOVE "t" TO Resulat (2:1)
           ELSE 
               MOVE "?" TO Resulat (2:1)
           END-IF.
           ADD 1 TO NbOfLettersCreated
           IF NbOfLettersCreated >= NbOfLetters
               DISPLAY "Calcul terminé : " Resulat
               STOP RUN
           END-IF.
           IF SEED (3:1) = "0" THEN
               MOVE "s" TO Resulat (3:1)
           ELSE IF SEED (3:1) = "1" THEN 
               MOVE "x" TO Resulat (3:1)
           ELSE IF SEED (3:1) = "2" THEN 
               MOVE "V" TO Resulat (3:1)
           ELSE IF SEED (3:1) = "3" THEN 
               MOVE "6" TO Resulat (3:1)
           ELSE IF SEED (3:1) = "4" THEN 
               MOVE "N" TO Resulat (3:1)
           ELSE IF SEED (3:1) = "5" THEN 
               MOVE "y" TO Resulat (3:1)
           ELSE IF SEED (3:1) = "6" THEN 
               MOVE "g" TO Resulat (3:1)
           ELSE IF SEED (3:1) = "7" THEN 
               MOVE "D" TO Resulat (3:1)
           ELSE IF SEED (3:1) = "8" THEN 
               MOVE "8" TO Resulat (3:1)
           ELSE IF SEED (3:1) = "9" THEN 
               MOVE "L" TO Resulat (3:1)
           ELSE 
               MOVE "?" TO Resulat (3:1)
           END-IF.
           ADD 1 TO NbOfLettersCreated
           IF NbOfLettersCreated >= NbOfLetters
               DISPLAY "Calcul terminé : " Resulat
               STOP RUN
           END-IF.
           IF SEED (4:1) = "0" THEN
               MOVE "B" TO Resulat (4:1)
           ELSE IF SEED (4:1) = "1" THEN
               MOVE "3" TO Resulat (4:1)
           ELSE IF SEED (4:1) = "2" THEN
               MOVE "A" TO Resulat (4:1)
           ELSE IF SEED (4:1) = "3" THEN
               MOVE "@" TO Resulat (4:1)
           ELSE IF SEED (4:1) = "4" THEN
               MOVE "$" TO Resulat (4:1)
           ELSE IF SEED (4:1) = "5" THEN
               MOVE "l" TO Resulat (4:1)
           ELSE IF SEED (4:1) = "6" THEN
               MOVE "w" TO Resulat (4:1)
           ELSE IF SEED (4:1) = "7" THEN
               MOVE "q" TO Resulat (4:1)
           ELSE IF SEED (4:1) = "8" THEN
               MOVE "R" TO Resulat (4:1)
           ELSE IF SEED (4:1) = "9" THEN
               MOVE "(" TO Resulat (4:1)
           ELSE 
               MOVE "?" TO Resulat (4:1)
           END-IF.
           ADD 1 TO NbOfLettersCreated
           IF NbOfLettersCreated >= NbOfLetters
               DISPLAY "Calcul terminé : " Resulat
               STOP RUN
           END-IF.
           IF SEED (5:1) = "0" THEN
               MOVE "z" TO Resulat (5:1)
           ELSE IF SEED (5:1) = "1" THEN 
               MOVE "!" TO Resulat (5:1)
           ELSE IF SEED (5:1) = "2" THEN 
               MOVE "S" TO Resulat (5:1)
           ELSE IF SEED (5:1) = "3" THEN 
               MOVE "O" TO Resulat (5:1)
           ELSE IF SEED (5:1) = "4" THEN 
               MOVE "9" TO Resulat (5:1)
           ELSE IF SEED (5:1) = "5" THEN 
               MOVE "f" TO Resulat (5:1)
           ELSE IF SEED (5:1) = "6" THEN 
               MOVE "n" TO Resulat (5:1)
           ELSE IF SEED (5:1) = "7" THEN 
               MOVE "E" TO Resulat (5:1)
           ELSE IF SEED (5:1) = "8" THEN 
               MOVE "o" TO Resulat (5:1)
           ELSE IF SEED (5:1) = "9" THEN 
               MOVE "%" TO Resulat (5:1)
           ELSE 
               MOVE "?" TO Resulat (5:1)
           END-IF.
           ADD 1 TO NbOfLettersCreated
           IF NbOfLettersCreated >= NbOfLetters
               DISPLAY "Calcul terminé : " Resulat
               STOP RUN
           END-IF.
           IF SEED (6:1) = "0" THEN
               MOVE "4" TO Resulat (6:1)
           ELSE IF SEED (6:1) = "1" THEN       
               MOVE "e" TO Resulat (6:1)
           ELSE IF SEED (6:1) = "2" THEN       
               MOVE "H" TO Resulat (6:1)
           ELSE IF SEED (6:1) = "3" THEN       
               MOVE ":" TO Resulat (6:1)
           ELSE IF SEED (6:1) = "4" THEN       
               MOVE "0" TO Resulat (6:1)
           ELSE IF SEED (6:1) = "5" THEN       
               MOVE "7" TO Resulat (6:1)
           ELSE IF SEED (6:1) = "6" THEN       
               MOVE "j" TO Resulat (6:1)
           ELSE IF SEED (6:1) = "7" THEN       
               MOVE "t" TO Resulat (6:1)
           ELSE IF SEED (6:1) = "8" THEN       
               MOVE ")" TO Resulat (6:1)
           ELSE IF SEED (6:1) = "9" THEN       
               MOVE "m" TO Resulat (6:1)
           ELSE 
               MOVE "?" TO Resulat (6:1)
           END-IF.
           ADD 1 TO NbOfLettersCreated
           IF NbOfLettersCreated >= NbOfLetters
               DISPLAY "Calcul terminé : " Resulat
               STOP RUN
           END-IF.
           IF SEED (7:1) = "0" THEN
               MOVE "^" TO Resulat (7:1)
           ELSE IF SEED (7:1) = "1" THEN
               MOVE "d" TO Resulat (7:1)
           ELSE IF SEED (7:1) = "2" THEN
               MOVE "=" TO Resulat (7:1)
           ELSE IF SEED (7:1) = "3" THEN
               MOVE "/" TO Resulat (7:1)
           ELSE IF SEED (7:1) = "4" THEN
               MOVE "~" TO Resulat (7:1)
           ELSE IF SEED (7:1) = "5" THEN
               MOVE "u" TO Resulat (7:1)
           ELSE IF SEED (7:1) = "6" THEN
               MOVE "-" TO Resulat (7:1)
           ELSE IF SEED (7:1) = "7" THEN
               MOVE "." TO Resulat (7:1)
           ELSE IF SEED (7:1) = "8" THEN
               MOVE "[" TO Resulat (7:1)
           ELSE IF SEED (7:1) = "9" THEN
               MOVE "|" TO Resulat (7:1)
           ELSE 
               MOVE "?" TO Resulat (7:1)
           END-IF.
           ADD 1 TO NbOfLettersCreated
           IF NbOfLettersCreated >= NbOfLetters
               DISPLAY "Calcul terminé : " Resulat
               STOP RUN
           END-IF.
           IF SEED (8:1) = "0" THEN
               MOVE "{" TO Resulat (8:1)
           ELSE IF SEED (8:1) = "1" THEN
               MOVE "v" TO Resulat (8:1)
           ELSE IF SEED (8:1) = "2" THEN
               MOVE "]" TO Resulat (8:1)
           ELSE IF SEED (8:1) = "3" THEN
               MOVE "3" TO Resulat (8:1)
           ELSE IF SEED (8:1) = "4" THEN
               MOVE "k" TO Resulat (8:1)
           ELSE IF SEED (8:1) = "5" THEN
               MOVE "#" TO Resulat (8:1)
           ELSE IF SEED (8:1) = "6" THEN
               MOVE "}" TO Resulat (8:1)
           ELSE IF SEED (8:1) = "7" THEN
               MOVE "+" TO Resulat (8:1)
           ELSE IF SEED (8:1) = "8" THEN
               MOVE "*" TO Resulat (8:1)
           ELSE IF SEED (8:1) = "9" THEN
               MOVE "J" TO Resulat (8:1)
           ELSE 
               MOVE "?" TO Resulat (8:1)
           END-IF.
           ADD 1 TO NbOfLettersCreated
           IF NbOfLettersCreated >= NbOfLetters
               DISPLAY "Calcul terminé : " Resulat
               STOP RUN
           END-IF.
           IF SEED (9:1) = "0" THEN
               MOVE "w" TO Resulat (9:1)
           ELSE IF SEED (8:1) = "1" THEN
               MOVE "h" TO Resulat (9:1)
           ELSE IF SEED (8:1) = "2" THEN
               MOVE "M" TO Resulat (9:1)
           ELSE IF SEED (8:1) = "3" THEN
               MOVE "B" TO Resulat (9:1)
           ELSE IF SEED (8:1) = "4" THEN
               MOVE "N" TO Resulat (9:1)
           ELSE IF SEED (8:1) = "5" THEN
               MOVE "D" TO Resulat (9:1)
           ELSE IF SEED (8:1) = "6" THEN
               MOVE "F" TO Resulat (9:1)
           ELSE IF SEED (8:1) = "7" THEN
               MOVE "Q" TO Resulat (9:1)
           ELSE IF SEED (8:1) = "8" THEN
               MOVE "S" TO Resulat (9:1)
           ELSE IF SEED (8:1) = "9" THEN
               MOVE "D" TO Resulat (9:1)
           ELSE 
               MOVE "8" TO Resulat (9:1)
           END-IF.
           ADD 1 TO NbOfLettersCreated
           IF NbOfLettersCreated >= NbOfLetters
               DISPLAY "Calcul terminé : " Resulat
               STOP RUN
           END-IF.
           IF SEED (10:1) = "0" THEN
               MOVE "q" TO Resulat (10:1)
           ELSE IF SEED (8:1) = "1" THEN
               MOVE "w" TO Resulat (10:1)
           ELSE IF SEED (8:1) = "2" THEN
               MOVE "e" TO Resulat (10:1)
           ELSE IF SEED (8:1) = "3" THEN
               MOVE "r" TO Resulat (10:1)
           ELSE IF SEED (8:1) = "4" THEN
               MOVE "t" TO Resulat (10:1)
           ELSE IF SEED (8:1) = "5" THEN
               MOVE "y" TO Resulat (10:1)
           ELSE IF SEED (8:1) = "6" THEN
               MOVE "u" TO Resulat (10:1)
           ELSE IF SEED (8:1) = "7" THEN
               MOVE "i" TO Resulat (10:1)
           ELSE IF SEED (8:1) = "8" THEN
               MOVE "o" TO Resulat (10:1)
           ELSE IF SEED (8:1) = "9" THEN
               MOVE "p" TO Resulat (10:1)
           ELSE 
               MOVE "?" TO Resulat (10:1)
           END-IF.
           ADD 1 TO NbOfLettersCreated
           IF NbOfLettersCreated >= NbOfLetters
               DISPLAY "Calcul terminé : " Resulat
               STOP RUN
           END-IF.
           STOP RUN
           goback.

       end program RandomGenerator.
