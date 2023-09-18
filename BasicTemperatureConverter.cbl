       program-id. BasicTemperatureConverter.

       environment division.
       configuration section.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       data division.
       working-storage section.
       01 ResponseOf1st PIC X(12).
       01 ValueOfFirst PIC S9(6)V9(9).
       01 ResponseOf2nd PIC X(12).
       01 Resultat PIC S9(6)V9(9).
       01 FACTOR1 PIC S9(1)V9(1) VALUE 1,8.
       01 FACTOR2 PIC 9(3)V9(2) VALUE 273,15.
       01 FACTOR3 PIC 9(2) VALUE 32.
       01 FACTOR4 PIC 9(1)V9(12) VALUE 0,555555555555.
       linkage section.

       procedure division.
           DISPLAY "WARNING : EN CAS DE VALEUR DECIMAL = ','"
           DISPLAY "Ceci est un convertisseur de température"
           DISPLAY "Valeur possiblement non exact"
           DISPLAY "Entrez la valeur de la première température"
           ACCEPT ValueOfFirst
           DISPLAY "Entrez CELSIUS/FAHRENHEIT/KELVIN"
           ACCEPT ResponseOf1st
           DISPLAY "Entrez CELSIUS/FAHRENHEIT/KELVIN pour la conversion"
           ACCEPT ResponseOf2nd

           IF ResponseOf1st = "CELSIUS" THEN
               IF ResponseOf2nd = "CELSIUS" THEN
                   DISPLAY "ERROR"
                   STOP RUN
               ELSE IF ResponseOf2nd = "FAHRENHEIT" THEN
                   COMPUTE Resultat = ValueOfFirst * FACTOR1
                   ADD FACTOR3 TO Resultat ROUNDED
                   DISPLAY "Le résultat est de : " Resultat
               ELSE IF ResponseOf2nd = "KELVIN" THEN
                   COMPUTE Resultat ROUNDED = ValueOfFirst + FACTOR2
                   DISPLAY "Le résultat est de : " Resultat
                   STOP RUN
               ELSE 
                   DISPLAY "Entrée non valide"
                   STOP RUN
               END-IF
           ELSE IF ResponseOf1st = "FAHRENHEIT" THEN
               IF ResponseOf2nd = "CELSIUS" THEN
                   COMPUTE Resultat = ValueOfFirst - FACTOR3
                   COMPUTE Resultat ROUNDED = Resultat * FACTOR1
                   DISPLAY "Le résultat est de : " Resultat
                   STOP RUN
               ELSE IF ResponseOf2nd = "FAHRENHEIT" THEN
                   DISPLAY "ERROR"
                   STOP RUN
               ELSE IF ResponseOf2nd = "KELVIN"
                   COMPUTE Resultat = ValueOfFirst - FACTOR3
                   COMPUTE Resultat = Resultat * FACTOR4
                   COMPUTE Resultat ROUNDED = Resultat + FACTOR2
                   DISPLAY "Le résultat est de : " Resultat
                   STOP RUN 
               END-IF
           ELSE IF ResponseOf1st = "KELVIN" THEN
               IF ResponseOf2nd = "CELSIUS" THEN 
                   COMPUTE Resultat = ValueOfFirst - FACTOR2
                   DISPLAY "Le résultat est de : " Resultat
                   STOP RUN
               ELSE IF ResponseOf2nd = "FAHRENHEIT" THEN
                   COMPUTE Resultat = ValueOfFirst - FACTOR2 
                   COMPUTE Resultat = Resultat * FACTOR1
                   COMPUTE Resultat ROUNDED = Resultat + FACTOR3
                   DISPLAY "Le résultat est de : " Resultat
                   STOP RUN
               ELSE IF ResponseOf2nd = "KElVIN"
                   DISPLAY "ERROR"
                   STOP RUN
               END-IF
           END-IF.
           goback.

       end program BasicTemperatureConverter.
       *>Celsius
       *>Fahrenheit
       *>Kelvin
       *>Début 16/09/2023 a 20H03
       *>Fin le 17/09/2023 a 7H50
       *>Temps de dev + test = 2 H
