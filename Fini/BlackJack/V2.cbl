       program-id. V2.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 CardToPick PIC 9(2).

       01 DealerValue PIC 9(2).

       01 P1Value PIC 9(2) VALUE 0.
       01 P1State PIC 9(1) VALUE 1.

       01 P2Value PIC 9(2).
       01 P2State PIC 9(1) VALUE 1.

       01 P3Value PIC 9(2) VALUE 0.
       01 P3State PIC 9(1) VALUE 1.
       
       01 P4Value PIC 9(2) VALUE 0.
       01 P4State PIC 9(1) VALUE 1.

       01 NbCardValue2 PIC 9(1) VALUE 4.
       01 NbCardValue3 PIC 9(1) VALUE 4.
       01 NbCardValue4 PIC 9(1) VALUE 0.
       01 NbCardValue5 PIC 9(1) VALUE 4.
       01 NbCardValue6 PIC 9(1) VALUE 4.
       01 NbCardValue7 PIC 9(1) VALUE 4.
       01 NbCardValue8 PIC 9(1) VALUE 4.
       01 NbCardValue9 PIC 9(1) VALUE 4.
       01 NbCardValue10 PIC 9(2) VALUE 16.*>10 + roi, reine, valet
       01 NbCardValue11 PIC 9(2) VALUE 4.*>as

       01 SEED PIC 9(2).

       01 UserResponse PIC X(3) VALUE NULL.
       01 NbOfPlayer PIC 9(1).
       linkage section.

       procedure division.
           *> premier tour de distribution
           DISPLAY "L'as vaut exclusivement 11 ici"
           DISPLAY "***************************************************"
           DISPLAY "Il faut avoir exclusivement plus que le croupier"
           DISPLAY "Sans dépasser 21 pour gagner"
           DISPLAY "***************************************************"
           DISPLAY "Si vous avez moins ou autant que le croupier et si"
           DISPLAY "Le croupier ne dépasse pas 21 vous perdez"
           DISPLAY "***************************************************"
           DISPLAY "Merci de ne pas tenir compte des"
           DISPLAY "Possible fautes d'orthographes"
           DISPLAY "***************************************************"
           DISPLAY "Merci de ne pas tenir compte de la possible"
           DISPLAY "Mal organisation des saut et affichage de certains"
           DISPLAY "Elements graphiques"
           DISPLAY "***************************************************"
           DISPLAY "                                                   "
           DISPLAY "Bienvenue dans le jeu du BlackJack"
           DISPLAY "***************************************************"
           DISPLAY "Entrez le nombre de joueur 1 à 4"
           ACCEPT NbOfPlayer
           IF NbOfPlayer <= 0 THEN
              DISPLAY "Pas assez de joueur"
              STOP RUN 
           ELSE IF NbOfPlayer >= 5 THEN
              DISPLAY "Trop de joueur"
              STOP RUN 
           END-IF.
           IF NbOfPlayer >= 1 THEN
              DISPLAY "Début du premier tour"
              DISPLAY "                                                "
              DISPLAY "************************************************"
              DISPLAY 'Entrez "1" pour distribuez une carte au joueur 1'
              ACCEPT UserResponse
              IF NOT UserResponse = 1 THEN
                 STOP RUN
              END-IF
              MOVE SPACES TO UserResponse
              PERFORM CardsCalculation
              COMPUTE P1Value = P1Value + CardToPick
              DISPLAY "Valeur du joueur 1 : " P1Value
              DISPLAY UserResponse
           END-IF.

           IF NbOfPlayer >= 2 THEN
              DISPLAY "************************************************"
              DISPLAY 'Entrez "2" pour distribuez une carte au joueur 2'
              ACCEPT UserResponse
              IF NOT UserResponse = 2 THEN
                 STOP RUN
              END-IF
              MOVE SPACES TO UserResponse
              PERFORM CardsCalculation
              COMPUTE P2Value = P2Value + CardToPick
              DISPLAY "Valeur du joueur 2 : " P2Value
              DISPLAY UserResponse
           END-IF.

           IF NbOfPlayer >= 3 THEN
              DISPLAY "************************************************"
              DISPLAY 'Entrez "3" pour distribuez une carte au joueur 3'
              ACCEPT UserResponse
              IF NOT UserResponse = 3 THEN
                 STOP RUN
              END-IF
              MOVE SPACES TO UserResponse
              PERFORM CardsCalculation
              COMPUTE P3Value = P3Value + CardToPick
              DISPLAY "Valeur du joueur 3 : " P3Value
              DISPLAY UserResponse
           END-IF.
           
           IF NbOfPlayer >= 4 THEN
              DISPLAY "************************************************"
              DISPLAY 'Entrez "4" pour distribuez une carte au joueur 4'
              ACCEPT UserResponse
              IF NOT UserResponse = 4 THEN
                 STOP RUN
              END-IF
              MOVE SPACES TO UserResponse
              PERFORM CardsCalculation
              COMPUTE P4Value = P4Value + CardToPick
              DISPLAY "Valeur du joueur 4 : " P4Value
              DISPLAY UserResponse
           END-IF.

           DISPLAY "***************************************************"
           DISPLAY 'Entrez "5" pour distribuez une carte au croupier'
           ACCEPT UserResponse
           IF NOT UserResponse = 5 THEN
              STOP RUN
           END-IF.
           MOVE SPACES TO UserResponse
           PERFORM CardsCalculation.
           COMPUTE DealerValue = DealerValue + CardToPick
           DISPLAY "Valeur du croupier : " DealerValue
           DISPLAY UserResponse
           DISPLAY "Fin du premier tour"
           DISPLAY "***************************************************"
           DISPLAY UserResponse
           DISPLAY "Valeur du joueur 1 : " P1Value
           DISPLAY "Valeur du joueur 2 : " P2Value
           DISPLAY "Valeur du joueur 3 : " P3Value
           DISPLAY "Valeur du joueur 4 : " P4Value
           DISPLAY "Valeur du croupier : " DealerValue
           DISPLAY UserResponse
           DISPLAY "***************************************************"
           DISPLAY UserResponse
           DISPLAY 'Entrez "2" pour passer au second tour'
           ACCEPT UserResponse
           IF NOT UserResponse = 2 THEN
              STOP RUN
           END-IF.

           *> deuxième tour de distribution
           IF NbOfPlayer >= 1 THEN
              DISPLAY "Début du second tour de distribution"
              DISPLAY "                                                "
              DISPLAY "************************************************"
              DISPLAY 'Entrez "1" pour distribuer une carte au joueur 1'
              ACCEPT UserResponse
              IF NOT UserResponse = 1 THEN
                 STOP RUN
              END-IF
              MOVE SPACES TO UserResponse
              PERFORM CardsCalculation
              COMPUTE P1Value = P1Value + CardToPick
              DISPLAY "Valeur du joueur 1 : " P1Value
              IF P1Value > 21 THEN
                 DISPLAY "                                             "
                 DISPLAY "*********************************************"
                 DISPLAY "Le joueur 1 perd"
                 DISPLAY "*********************************************"
                 DISPLAY "                                             "
                 MOVE 0 TO P1State
              ELSE IF P1Value = 21 THEN
                 DISPLAY UserResponse
                 DISPLAY "*********************************************"
                 DISPLAY "Score du joueur 1 : " P1Value
                 DISPLAY "BlackJack du joueur 1"
                 MOVE 0 TO P1State
                 DISPLAY "*********************************************"
                 DISPLAY UserResponse
              END-IF
              DISPLAY UserResponse
           END-IF.
           
           IF NbOfPlayer >= 2 THEN
              DISPLAY "************************************************"
              DISPLAY 'Entrez "2" pour distribuez une carte au joueur 2'
              ACCEPT UserResponse 
              IF NOT UserResponse = 2 THEN
                 STOP RUN
              END-IF
              MOVE SPACES TO UserResponse
              PERFORM CardsCalculation
              COMPUTE P2Value = P2Value + CardToPick
              DISPLAY "Valeur du joueur 2 : " P2Value
              IF P2Value > 21 THEN
                 DISPLAY "                                             "
                 DISPLAY "*********************************************"
                 DISPLAY "Le joueur 2 perd"
                 DISPLAY "*********************************************"
                 DISPLAY "                                             "
                 MOVE 0 TO P2State
              ELSE IF P2Value = 21 THEN
                 DISPLAY UserResponse 
                 DISPLAY "*********************************************"
                 DISPLAY "Score du joueur 2 : " P2Value
                 DISPLAY "BlackJack du joueur 2"
                 MOVE 0 TO P2State
                 DISPLAY "*********************************************"
                 DISPLAY UserResponse
              END-IF
           DISPLAY UserResponse
           END-IF.
           
           IF NbOfPlayer >= 3 THEN
              DISPLAY "************************************************"
              DISPLAY 'Entrez "3" pour distribuer une carte au joueur 3'
              ACCEPT UserResponse
              IF NOT UserResponse = 3 THEN
                 STOP RUN
              END-IF.
              MOVE SPACES TO UserResponse
              PERFORM CardsCalculation
              COMPUTE P3Value = P3Value + CardToPick
              DISPLAY "Valeur du joueur 3 : " P3Value
              IF P3Value > 21 THEN
                 DISPLAY "                                             "
                 DISPLAY "*********************************************"
                 DISPLAY "Le joueur 3 perd"
                 DISPLAY "*********************************************"
                 DISPLAY "                                             "
                 MOVE 0 TO P3State
              ELSE IF P3Value = 21 THEN
                 DISPLAY UserResponse
                 DISPLAY "*********************************************"
                 DISPLAY "Score du joueur 3 : " P3Value
                 DISPLAY "BlackJack du joueur 3"
                 MOVE 0 TO P3State
                 DISPLAY "*********************************************"
                 DISPLAY UserResponse
              END-IF
              DISPLAY UserResponse
           END-IF.

           IF NbOfPlayer >= 4 THEN
              DISPLAY "************************************************"
              DISPLAY 'Entrez "4" pour distribuer une carte au joueur 4'
              ACCEPT UserResponse 
              IF NOT UserResponse = 4 THEN
                 STOP RUN
              END-IF
              MOVE SPACES TO UserResponse
              PERFORM CardsCalculation.
              COMPUTE P4Value = P4Value + CardToPick
              DISPLAY "Valeur du joueur 4 : " P4Value
              IF P4Value > 21 THEN
                 DISPLAY "                                             "
                 DISPLAY "*********************************************"
                 DISPLAY "Le joueur 4 perd"
                 DISPLAY "*********************************************"
                 DISPLAY "                                             "
                 MOVE 0 TO P4State
              ELSE IF P4Value = 21 THEN
                 DISPLAY UserResponse
                 DISPLAY "*********************************************"
                 DISPLAY "Score du joueur 4 : " P4Value
                 DISPLAY "BlackJack du joueur 4"
                 MOVE 0 TO P4State
                 DISPLAY "*********************************************"
                 DISPLAY UserResponse
              END-IF
              DISPLAY UserResponse
           END-IF.

           DISPLAY "***************************************************"
           DISPLAY 'Entrez "5" pour distribuer une carte au croupier'
           ACCEPT UserResponse
           IF NOT UserResponse = 5 THEN
              STOP RUN
           END-IF.
           MOVE SPACES TO UserResponse
           PERFORM CardsCalculation.
           COMPUTE DealerValue = DealerValue + CardToPick
           DISPLAY "Valeur du croupier caché"

           DISPLAY UserResponse
           DISPLAY "Fin du second tour"
           DISPLAY "***************************************************"
           DISPLAY UserResponse
           DISPLAY "Valeur du joueur 1 : " P1Value
           DISPLAY "Valeur du joueur 2 : " P2Value
           DISPLAY "Valeur du joueur 3 : " P3Value
           DISPLAY "Valeur du joueur 4 : " P4Value
           DISPLAY "Valeur du croupier : " "Valeur cachée"
           DISPLAY UserResponse
           DISPLAY "***************************************************"
           DISPLAY UserResponse
           DISPLAY 'Entrez "3" pour passer au troisième tour'
           ACCEPT UserResponse
           IF NOT UserResponse = 3 THEN
              STOP RUN
           END-IF.

           *>troisième et dernier tour
           DISPLAY "Début du troisième tour de distribution"
           IF P1State = 1 AND NbOfPlayer >= 1 THEN
              GO TO Player1Turn
              ELSE 
                 IF P2State = 1 AND NbOfPlayer >= 2 THEN
                    GO TO Player2Turn
                    ELSE 
                       IF P3State = 1 AND NbOfPlayer >= 3 THEN
                          GO TO Player3Turn
                          ELSE 
                             IF P4State = 1 AND NbOfPlayer >= 4 THEN
                                GO TO Player4Turn
                              END-IF
                        END-IF
                  END-IF
            END-IF.
       GOBACK.
           CardsCalculation.
              MOVE FUNCTION CURRENT-DATE (15:16) TO SEED(1:2)
              COMPUTE SEED = SEED * 5
              IF SEED >= 0 AND SEED <= 9 AND NbCardValue10 >= 1 
                 THEN
                 MOVE NULL TO CardToPick
                 MOVE 10 TO CardToPick
              ELSE IF SEED >= 10 AND SEED <= 19 AND NbCardValue9 >=1
                 THEN
                 MOVE NULL TO CardToPick
                 MOVE 09 TO CardToPick
              ELSE IF SEED >= 20 AND SEED <= 29 AND NbCardValue6 >= 1
                 THEN
                 MOVE NULL TO CardToPick
                 MOVE 06 TO CardToPick
              ELSE IF SEED >= 30 AND SEED <= 39 AND NbCardValue3 >= 1
                 THEN
                 MOVE NULL TO CardToPick
                 MOVE 03 TO CardToPick
              ELSE IF SEED >= 40 AND SEED <= 49 AND NbCardValue5 >= 1
                 THEN
                 MOVE NULL TO CardToPick
                 MOVE 05 TO CardToPick
              ELSE IF SEED >= 50 AND SEED <= 59 AND NbCardValue8 >= 1
                 THEN
                 MOVE NULL TO CardToPick
                 MOVE 08 TO CardToPick
              ELSE IF SEED >= 60 AND SEED <= 69 AND NbCardValue7 >= 1
                 THEN
                 MOVE NULL TO CardToPick
                 MOVE 07 TO CardToPick
              ELSE IF SEED >= 70 AND SEED <= 79 AND NbCardValue4 >= 1
                 THEN
                 MOVE NULL TO CardToPick
                 MOVE 04 TO CardToPick
              ELSE IF SEED >= 80 AND SEED <= 89 AND NbCardValue11 >= 1
                 THEN
                 MOVE NULL TO CardToPick
                 MOVE 11 TO CardToPick
              ELSE IF SEED >= 90 AND SEED <= 99 AND NbCardValue2 >= 1
                 THEN
                 MOVE NULL TO CardToPick
                 MOVE 02 TO CardToPick
              ELSE 
                 GO TO CardsCalculation
              END-IF.
              IF CardToPick = 02 AND NbCardValue2 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue2
              ELSE IF CardToPick = 03 AND NbCardValue3 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue3
              ELSE IF CardToPick = 04 AND NbCardValue4 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue4
              ELSE IF CardToPick = 05 AND NbCardValue5 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue5
              ELSE IF CardToPick = 06 AND NbCardValue6 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue6
              ELSE IF CardToPick = 07 AND NbCardValue7 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue7
              ELSE IF CardToPick = 08 AND NbCardValue8 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue8
              ELSE IF CardToPick = 09 AND NbCardValue9 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue9
              ELSE IF CardToPick = 10 AND NbCardValue10 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue10
              ELSE IF CardToPick = 11 AND NbCardValue11 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue11
              END-IF.
           Player1Turn.
              MOVE SPACES TO UserResponse
              DISPLAY UserResponse
              DISPLAY "************************************************"
              IF P1Value > 21 THEN
                 IF P2State = 1 AND NbOfPlayer >= 2 THEN
                    GO TO Player2Turn
                  ELSE
                    IF P3State = 1 AND NbOfPlayer >= 3 THEN
                       GO TO Player3Turn
                     ELSE 
                       IF P4State = 1 AND NbOfPlayer >= 4 THEN
                          GO TO Player4Turn
                        ELSE
                          GO TO DealerTurn
                        END-IF
                     END-IF
                  END-IF
              END-IF.
              DISPLAY 'Tour du joueur 1'
              DISPLAY 'Votre valeur actuelle est de : ' P1Value
              DISPLAY 'Pour tirer plus de carte entrez "1"'
              DISPLAY 'Pour ne pas tirer pas de carte entrez "0"'
              ACCEPT UserResponse
              IF UserResponse = 1 THEN
                 PERFORM CardsCalculation
                 COMPUTE P1Value = P1Value + CardToPick
                 DISPLAY "                                             "
                 DISPLAY "*********************************************"
                 DISPLAY "Score du joueur 1 : " P1Value
                 DISPLAY "*********************************************"
                 DISPLAY "                                             "
                 IF P1Value > 21 THEN
                    DISPLAY "                                          "
                    DISPLAY "******************************************"
                    DISPLAY "Le joueur 1 perd"
                    DISPLAY "******************************************"
                    DISPLAY "                                          "
                    MOVE 0 TO P1State
                    DISPLAY "******************************************"
                    IF P2State = 1 AND NbOfPlayer >= 2 THEN
                       PERFORM Player2Turn
                       ELSE 
                          IF P3State = 1 AND NbOfPlayer >= 3 THEN
                             GO TO Player3Turn
                             ELSE 
                                IF P4State = 1 AND NbOfPlayer >= 4 THEN
                                   GO TO Player4Turn
                                 ELSE 
                                   GO TO DealerTurn
                                 END-IF
                           END-IF
                     END-IF
                 END-IF 
                 GO TO Player1Turn
               ELSE IF UserResponse = 0 THEN
                  IF P2State = 1 AND NbOfPlayer >= 2 THEN
                     PERFORM Player2Turn
                     ELSE 
                        IF P3State = 1 AND NbOfPlayer >= 3 THEN
                           GO TO Player3Turn
                           ELSE 
                           IF P4State = 1 AND NbOfPlayer >= 4 THEN
                              GO TO Player4Turn
                              ELSE 
                                 GO TO DealerTurn
                           END-IF
                        END-IF
                  GO TO Player1Turn
               END-IF. 
               DISPLAY "                                              ".
           Player2Turn.
              MOVE SPACE TO UserResponse
              DISPLAY "************************************************"
              DISPLAY "Tour du joueur 2"
              DISPLAY "Votre valeur actuelle est de : " P2Value
              DISPLAY 'Pour tirer plus de carte entrez "1"'
              DISPLAY 'Pour ne plus tirer de carte entrez "0"'
              ACCEPT UserResponse
              IF UserResponse = 1 THEN
                 PERFORM CardsCalculation
                 COMPUTE P2Value = P2Value + CardToPick
                 DISPLAY "                                             "
                 DISPLAY "*********************************************"
                 DISPLAY "Score du joueur 2 : " P2Value
                 DISPLAY "*********************************************"
                 DISPLAY "                                             "
                 IF P2Value > 21 THEN
                    DISPLAY "Le joueur 2 perd"
                    DISPLAY "******************************************"
                    DISPLAY "                                          "
                    MOVE 0 TO P2State
                    IF P3State = 1 AND NbOfPlayer >= 3 THEN
                       GO TO Player3Turn
                       ELSE 
                          IF P4State = 1 AND NbOfPlayer >= 4 THEN
                             GO TO Player4Turn
                           ELSE
                             GO TO DealerTurn
                           END-IF
                     END-IF
                  END-IF
              GO TO Player2Turn
              ELSE IF UserResponse = 0 THEN
                    IF P3State = 1 AND NbOfPlayer >= 3 THEN
                       GO TO Player3Turn
                       ELSE 
                          IF P4State = 1 AND NbOfPlayer >= 4 THEN
                             GO TO Player4Turn
                           ELSE
                             GO TO DealerTurn
                           END-IF
                     END-IF
              END-IF.
              DISPLAY "                                               ".
           Player3Turn. 
              MOVE SPACE TO UserResponse
              DISPLAY "************************************************"
              DISPLAY "Tour du joueur 3"
              DISPLAY "Votre valeur actuelle est de : " P3Value
              DISPLAY 'Pour tirer plus de carte entrez "1"'
              DISPLAY 'Pour ne pas tirer plus de carte entrez "0"'
              ACCEPT UserResponse
              IF UserResponse = 1 THEN
                 PERFORM CardsCalculation
                 COMPUTE P3Value = P3Value + CardToPick
                 DISPLAY "                                             "
                 DISPLAY "*********************************************"
                 DISPLAY "Score du joueur 3 : " P3Value
                 DISPLAY "*********************************************"
                 DISPLAY "                                             "
                 IF P3Value > 21 THEN
                    MOVE 0 TO P3State
                    DISPLAY "Le joueur 3 perd"
                    DISPLAY "                                          "
                    IF P4State = 1 AND NbOfPlayer >= 4 THEN
                       GO TO Player4Turn
                    ELSE 
                       GO TO DealerTurn
                    END-IF
                 END-IF
               GO TO Player3Turn
               ELSE IF UserResponse = 0 THEN
                 IF P4State = 1 AND NbOfPlayer >= 4 THEN  
                    GO TO Player4Turn
                    ELSE 
                       GO TO DealerTurn
                 END-IF
              END-IF.
              DISPLAY "                                               ".
           Player4Turn.
              MOVE SPACE TO UserResponse
              DISPLAY "************************************************"
              DISPLAY "Tour du joueur 4"
              DISPLAY "Votre valeur actuelle est de : " P4Value
              DISPLAY 'Pour tirer plus de carte entrez "1"'
              DISPLAY 'Pour ne pas tirer plus de carte entrez "0"'
              ACCEPT UserResponse
              IF UserResponse = 1 THEN
                 PERFORM CardsCalculation
                 COMPUTE P4Value = P4Value + CardToPick
                 DISPLAY "                                             "
                 DISPLAY "*********************************************"
                 DISPLAY "Valeur du joueur 4 : " P4Value
                 DISPLAY "*********************************************"
                 DISPLAY "                                             "
                 IF P4Value > 21 THEN
                    MOVE 0 TO P4State
                    GO TO DealerTurn
                 END-IF
              GO TO Player4Turn
              ELSE IF UserResponse = 0 THEN
                 GO TO DealerTurn
              END-IF.
           DealerTurn.
              MOVE SPACE TO UserResponse
              DISPLAY "Le croupier retourne ses cartes"
              DISPLAY "La somme total du croupier est de : " DealerValue
              IF DealerValue > 21 THEN
                 DISPLAY "Le croupier perd"
              END-IF.
              PERFORM UNTIL DealerValue >= 16
                 DISPLAY "Le croupier pioche"
                 PERFORM CardsCalculation
                 COMPUTE DealerValue = DealerValue + CardToPick
                 DISPLAY "Valeur actuelle du croupier : " DealerValue
                 DISPLAY "                                             "
              END-PERFORM.
              DISPLAY "Le croupier ne pioche plus"
              DISPLAY "Valeur total du croupier : " DealerValue
              IF DealerValue > 21 THEN
                 DISPLAY "Le croupier perd"
              END-IF.
              *> marche pas :(
              *>IF DealerValue < 22 THEN
                 *>IF NbOfPlayer = 1 THEN *> si 1 joueur
                    *>IF P1State = 1 THEN
                       *>IF P1Value <= DealerValue THEN
                          *>DISPLAY "Le joueur 1 perd face au croupier"
                       *>ELSE IF P1Value > DealerValue THEN
                          *>DISPLAY "Le joueur 1 gagne face au croupier"
                       *>END-IF
                    *>END-IF
                 *>END-IF
                 *>IF NbOfPlayer = 2 THEN *> si 2 joueurs
                    *>IF P1State = 1 THEN
                       *>IF P1Value <= DealerValue THEN
                          *>DISPLAY "Le joueur 1 perd face au croupier"
                       *>ELSE IF P1Value > DealerValue THEN
                          *>DISPLAY "Le joueur 1 gagne face au croupier"
                       *>END-IF
                    *>END-IF
                    *>IF P2State = 1 THEN
                       *>IF P2Value <= DealerValue THEN
                          *>DISPLAY "Le joueur 2 perd face au croupier"
                       *>ELSE IF P2Value > DealerValue THEN
                          *>DISPLAY "Le joueur 2 gagne face au croupier"
                       *>END-IF
                    *>END-IF
                 *>END-IF
                 *>IF NbOfPlayer = 3 THEN *> si 3 joueurs
                    *>IF P1State = 1 THEN
                       *>IF P1Value <= DealerValue THEN
                          *>DISPLAY "Le joueur 1 perd face au croupier"
                       *>ELSE IF P1Value > DealerValue THEN
                          *>DISPLAY "Le joueur 1 gagne face au croupier"
                       *>END-IF
                    *>END-IF
                    *>IF P2State = 1 THEN
                       *>IF P2Value <= DealerValue THEN
                          *>DISPLAY "Le joueur 2 perd face au croupier"
                       *>ELSE IF P2Value > DealerValue THEN
                          *>DISPLAY "Le joueur 2 gagne face au croupier"
                       *>END-IF
                    *>END-IF
                    *>IF P3State = 1 THEN 
                       *>IF P3Value <= DealerValue THEN
                          *>DISPLAY "Le joueur 3 perd face au croupier"
                       *>ELSE IF P3Value > DealerValue THEN
                          *>DISPLAY "Le joueur 3 gagne face au croupier"
                       *>END-IF
                    *>END-IF
                 *>IF NbOfPlayer = 4 THEN *> si 4 joueurs
                    *>IF P1State = 1 THEN
                       *>IF P1Value <= DealerValue THEN
                          *>DISPLAY "Le joueur 1 perd face au croupier"
                       *>ELSE IF P1Value > DealerValue THEN
                          *>DISPLAY "Le joueur 1 gagne face au croupier"
                       *>END-IF
                    *>END-IF
                    *>IF P2State = 1 THEN
                       *>IF P2Value <= DealerValue THEN
                          *>DISPLAY "Le joueur 2 perd face au croupier"
                       *>ELSE IF P2Value > DealerValue THEN
                          *>DISPLAY "Le joueur 2 gagne face au croupier"
                       *>END-IF
                    *>END-IF
                    *>IF P3State = 1 THEN
                       *>IF P3Value <= DealerValue THEN
                          *>DISPLAY "Le joueur 3 perd face au croupier"
                       *>ELSE IF P3Value > DealerValue THEN
                          *>DISPLAY "Le joueur 3 gagne face au croupier"
                       *>END-IF
                    *>END-IF
                    *>IF P4State = 1 THEN
                       *>IF P4Value <= DealerValue THEN
                          *>DISPLAY "Le joueur 4 perd face au croupier"
                       *>ELSE IF P4Value > DealerValue THEN
                          *>DISPLAY "Le joueur 4 gagne face au croupier"
                       *>END-IF
                    *>END-IF
              *>END-IF.
              IF NbOfPlayer >= 1 THEN
                 DISPLAY "Valeur du joueur 1 : " P1Value
              END-IF.
              IF NbOfPlayer >= 2 THEN
                 DISPLAY "Valeur du joueur 2 : " P2Value
              END-IF.
              IF NbOfPlayer >= 3 THEN
                 DISPLAY "Valeur du joueur 3 : " P3Value
              END-IF.
              IF NbOfPlayer >= 4 THEN
                 DISPLAY "Valeur du joueur 4 : " P4Value
              END-IF.
              DISPLAY "                                                "
              DISPLAY "Si le croupier a plus"
              DISPLAY "Ou autant qu'un joueur sous 21 il gagne"
              DISPLAY '                                                '
              DISPLAY "Si le croupier a dépassé 21 il perd"
              DISPLAY "                                               "
              DISPLAY "Si un joueur a plus que le croupier"
              DISPLAY "Sans dépasser 21 il gagne".
         end program V2.
