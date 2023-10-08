       program-id. untitled.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 TestValue PIC 9(9) VALUE 1.
       01 Card PIC 9(6).
       01 Value2 PIC 9(1) VALUE 4.
       01 Value3 PIC 9(1) VALUE 4.
       01 Value4 PIC 9(1) VALUE 4.
       01 Value5 PIC 9(1) VALUE 4.
       01 Value6 PIC 9(1) VALUE 4.
       01 Value7 PIC 9(1) VALUE 4.
       01 Value8 PIC 9(1) VALUE 4.
       01 Value9 PIC 9(1) VALUE 4.
       01 Value10 PIC 9(2) VALUE 16.
       01 Value11 PIC 9(2) VALUE 4.
       01 CardToPick PIC 9(1) VALUE 1.
       01 NumberOfPlayer PIC 9(1).
       01 UserResponse PIC X(18).
       01 NumberOfCardsDistributed PIC 9(1).
       01 DealerValue PIC 9(2) VALUE 0.
       01 Player1Value PIC 9(2) VALUE 0.
       01 Player2Value PIC 9(2) VALUE 0.
       01 Player3Value PIC 9(2) VALUE 0.
       01 Player4Value PIC 9(2) VALUE 0.
       linkage section.
       procedure division.
           DISPLAY "L'as vaut 11 non modifiable"
           DISPLAY "L'ordinateur est le croupier"
           DISPLAY "Nombre de joueurs max 4"
           ACCEPT NumberOfPlayer
           IF NumberOfPlayer > 4 THEN
              DISPLAY "Trop de joueur"
           ELSE IF NumberOfPlayer < 1 THEN
              DISPLAY "Pas assez de joueur"
           END-IF
           DISPLAY 'Entrez "PLAY" pour commencer à jouer'
           ACCEPT UserResponse
           IF UserResponse = "PLAY" THEN
              PERFORM GiveCardToDealer
              MOVE SPACE TO UserResponse
           ELSE 
              DISPLAY "Réponse invalide fin du programme"
           END-IF.
       DistributeCards.
           ADD 7 TO CardToPick
           IF CardToPick = "0" THEN *>0 = les cartes a valeurs 10
               DISPLAY CardToPick
               IF Value10 >= 1 THEN
                   COMPUTE Value10 = Value10 - 1
               END-IF
           ELSE IF CardToPick = "1" THEN *>les as
               IF Value11 >= 1 THEN
                   COMPUTE Value11 = Value11 - 1
               END-IF           
           ELSE IF CardToPick = "2" THEN
               IF Value2 >= 1 THEN
                   COMPUTE Value2 = Value2 - 1
               END-IF
           ELSE IF CardToPick = "3" THEN
               IF Value3 >= 1 THEN
                   COMPUTE Value3 = Value3 - 1
               END-IF
           ELSE IF CardToPick = "4" THEN
               IF Value4 >= 1 THEN
                   COMPUTE Value4 = Value4 - 1
               END-IF
           ELSE IF CardToPick = "5" THEN   
               IF Value5 >= 1 THEN
                   COMPUTE Value5 = Value5 - 1
               END-IF
           ELSE IF CardToPick = "6" THEN
               IF Value6 >= 1 THEN
                   COMPUTE Value6 = Value6 - 1
               END-IF
           ELSE IF CardToPick = "7" THEN
               IF Value7 >= 1 THEN
                   COMPUTE Value7 = Value7 - 1
               END-IF
           ELSE IF CardToPick = "8" THEN
               IF Value8 >= 1 THEN
                   COMPUTE Value8 = Value8 - 1
               END-IF
           ELSE IF CardToPick = "9" THEN
               IF Value9 >= 1 THEN
                   COMPUTE Value9 = Value9 - 1
               END-IF
           END-IF.
       GiveCardToDealer.
           PERFORM DistributeCards.
           DISPLAY DealerValue
           COMPUTE DealerValue = CardToPick + DealerValue
           DISPLAY DealerValue
           DISPLAY 'fin du tour du croupier'
           MOVE SPACE TO UserResponse
           DISPLAY "Player1 pour le tour du joueur 1"
           ACCEPT UserResponse
           IF UserResponse = "Player1" THEN
              PERFORM GiveCardToPlayer1
           END-IF.
       GiveCardToPlayer1.
           DISPLAY "gufdghj"
           COMPUTE Player1Value = Player1Value + CardToPick
           IF NumberOfPlayer > 1 THEN
              PERFORM GiveCardToPlayer2
           END-IF.
       GiveCardToPlayer2. 
           COMPUTE Player2Value = Player2Value + CardToPick
           IF NumberOfPlayer > 2 THEN
              PERFORM GiveCardToPlayer3
           END-IF.
       GiveCardToPlayer3.
           COMPUTE Player3Value = Player3Value + CardToPick
           IF NumberOfPlayer > 3 THEN
              PERFORM GiveCardToPlayer4
           END-IF.
       GiveCardToPlayer4.
       COMPUTE Player4Value = Player4Value + CardToPick
           IF NumberOfPlayer > 4 THEN
              DISPLAY "fin de la première distribution"
           END-IF.
       end program untitled.
