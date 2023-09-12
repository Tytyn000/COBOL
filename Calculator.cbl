       IDENTIFICATION DIVISION.
       program-id. Calculator.

       environment division.
       configuration section.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       data division.
       working-storage section.
       01 Number1 PIC S9(6)V9(6).
       01 Number2 PIC S9(6)V9(6).
       01 Resultat PIC S9(6)V9(6).
       01 PI PIC 9(1)V9(15) VALUE 3,141592653589793.
       01 Signe PIC X(1) VALUE SPACE.
       77 UserResponse PIC X(16).
       linkage section.
                                                                             
       procedure division.
       DISPLAY "EN CAS DE VALEUR DECIMAL UTILSER , POUR LA SEPARATION"  
       DISPLAY "Le . n'est pas un séparateur Entier,Decimal"
       DISPLAY "Pour faire une racine carré écrivez SquareRoot"
       DISPLAY "Sinon ignorez la ligne du dessus et appuyez sur ENTER"
       ACCEPT UserResponse.
       IF UserResponse = "SquareRoot" THEN
           DISPLAY "Entrez le chiffre"
           ACCEPT Number1
           IF Number1 LESS THAN OR EQUAL TO 0 THEN
               DISPLAY "Racine non calculable"
               STOP RUN
           END-IF
           COMPUTE Resultat ROUNDED = FUNCTION SQRT (Number1)
           DISPLAY "Le résultat est : " Resultat
           STOP RUN
       END-IF.
       DISPLAY "Entrez le premier nombre(si négatif ajouté)". 
       ACCEPT Number1.
       DISPLAY "Entrez le signe de l'opération(+, -, *, /, √)".
       ACCEPT Signe.
       DISPLAY "Entrez le deuxieme nombre".
       ACCEPT Number2.
       IF Signe = "+" THEN
           COMPUTE Resultat ROUNDED = Number1 + Number2
              ON SIZE ERROR
                 DISPLAY "ERREUR D'ADDITION"
                 STOP RUN
        ELSE IF Signe = "-" THEN
           COMPUTE Resultat ROUNDED = Number1 - Number2
              ON SIZE ERROR
                 DISPLAY "ERREUR DE SOUSTRACTION"
                 STOP RUN
        ELSE IF Signe = "*" THEN
           COMPUTE Resultat ROUNDED = Number1 * Number2
              ON SIZE ERROR
                 DISPLAY "ERREUR DE MULTIPLICATION"
                 STOP RUN
        ELSE IF Signe = "/" THEN
           COMPUTE Resultat ROUNDED = Number1 / Number2
           ON SIZE ERROR
              DISPLAY "ERREUR DE DIVISION"
              STOP RUN
        ELSE
           DISPLAY "SIGNE INVALIDE"
           STOP RUN
       END-IF.
       DISPLAY "Valeur possiblement non exact car arrondi a l'affichage"
       DISPLAY "Le résultat est de " Resultat.
       STOP RUN.
       end program Calculator.
