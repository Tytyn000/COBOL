       program-id. Sequence.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 PreviousNb PIC 9(38) VALUE 1.
       01 2PreviousNb PIC 9(38) VALUE 0.
       01 Ar OCCURS 99 TIMES PIC 9(38).
       01 NbToShow PIC 9(38).
       01 NbToCreate PIC 9(3).
       01 NbCreated PIC 9(3).
       01 UserResponse PIC 9(38).
       linkage section.

       procedure division.
           DISPLAY "Ceci calcule la séquence de Fibonacci"
           DISPLAY "Ne peut pas calculer un nombre > 38 chiffres"
           DISPLAY "ATTENTION : Pas de troncature sur le résultat"
           DISPLAY "38 chiffres seront affichés"
           DISPLAY "Y compris les 0 quand la valeur est nulle"
           DISPLAY "Peut générer max 999 chiffres"
           DISPLAY "Combien de chiffre à générer ?"
           ACCEPT NbToCreate
           IF (NbToCreate > 999)
              DISPLAY "Nb trop élevé"
              STOP RUN
           END-IF.
           DISPLAY 'Pour obtenir une réponse avec les valeurs "0" : '
           DISPLAY 'Pour obtenir la valeur + phrase + espace "1" :'
           ACCEPT UserResponse
           IF UserResponse = 0
              PERFORM CalculateSequence
           ELSE IF UserResponse = 1
              PERFORM CalculateSequenceWithText
           ELSE
              PERFORM CalculateSequence
           END-IF.
           goback.
       
       CalculateSequence.
           PERFORM NbToCreate TIMES
              ADD 1 TO NbCreated
              COMPUTE NbToShow = PreviousNb + 2PreviousNb
              COMPUTE 2PreviousNb = PreviousNb
              COMPUTE PreviousNb = NbToShow
              DISPLAY NbToShow
           END-PERFORM.
       CalculateSequenceWithText.
           PERFORM NbToCreate TIMES
              ADD 1 TO NbCreated
              DISPLAY "************************************************"
              COMPUTE NbToShow = PreviousNb + 2PreviousNb
              DISPLAY "Numéro de calcul : " NbCreated 
              DISPLAY "La valeur est de : " NbToShow
              DISPLAY "Elle vient de : " PreviousNb " + " 2PreviousNb
              COMPUTE 2PreviousNb = PreviousNb
              COMPUTE PreviousNb = NbToShow
           END-PERFORM.

       end program Sequence.
