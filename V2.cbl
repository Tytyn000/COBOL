       program-id. V2.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 CardToPick PIC 9(2).
       01 ValueOfCard PIC 9(1).
       01 CurrentDistributionCardTurn PIC 9(2) VALUE 0.

       01 DealerValue PIC 9(2).

       01 P1Value PIC 9(2).
       01 P1State PIC 9(1) VALUE 1.

       01 P2Value PIC 9(2).
       01 P2State PIC 9(1) VALUE 1.

       01 P3Value PIC 9(2).
       01 P3State PIC 9(1) VALUE 1.

       01 P4Value PIC 9(2).
       01 P4State PIC 9(1) VALUE 1.

       01 NbCardValue2 PIC 9(1) VALUE 4.
       01 NbCardValue3 PIC 9(1) VALUE 4.
       01 NbCardValue4 PIC 9(1) VALUE 4.
       01 NbCardValue5 PIC 9(1) VALUE 4.
       01 NbCardValue6 PIC 9(1) VALUE 4.
       01 NbCardValue7 PIC 9(1) VALUE 4.
       01 NbCardValue8 PIC 9(1) VALUE 4.
       01 NbCardValue9 PIC 9(1) VALUE 4.
       01 NbCardValue10 PIC 9(2) VALUE 16.*>10 + roi, reine, valet
       01 NbCardValue11 PIC 9(1) VALUE 4.*>as
       01 TestValue PIC 9(1).

       01 SEED PIC 9(1).
       linkage section.

       procedure division.
           PERFORM DealerTurn.
           CardsCalculation.
              MOVE FUNCTION CURRENT-DATE (16:1) TO SEED
              ADD SEED TO CardToPick
              ADD 1 TO CardToPick
              IF CardToPick = "0" AND NbCardValue10 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue10
              ELSE IF CardToPick = "01" AND NbCardValue11 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue11
              ELSE IF CardToPick = "02" AND NbCardValue2 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue2
              ELSE IF CardToPick = "03" AND NbCardValue3 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue3
              ELSE IF CardToPick = "04" AND NbCardValue4 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue4
              ELSE IF CardToPick = "05" AND NbCardValue5 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue5
              ELSE IF CardToPick = "06" AND NbCardValue6 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue6
              ELSE IF CardToPick = "07" AND NbCardValue7 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue7
              ELSE IF CardToPick = "08" AND NbCardValue8 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue8
              ELSE IF CardToPick = "09" AND NbCardValue9 >= 1 THEN
                 SUBTRACT 1 FROM NbCardValue9
              ELSE IF CardToPick >= 10 THEN
                 PERFORM CardsCalculation
              END-IF.
           DealerTurn.
              ADD 1 TO CurrentDistributionCardTurn.
              PERFORM CardsCalculation.
              COMPUTE DealerValue = DealerValue + CardToPick
              
              IF CurrentDistributionCardTurn = "02" THEN
                 DISPLAY "Carte distribué au croupier mais caché"
              ELSE
                 DISPLAY "Carte distribué au croupier" 
                 DISPLAY "Somme total du croupier = : " DealerValue
              END-IF.
              IF P1State = "1" THEN
                 PERFORM Player1Turn
              END-IF.
           Player1Turn.
              PERFORM CardsCalculation.
              IF CardToPick = "0" OR "1" THEN
                 MOVE 10 TO CardToPick
              END-IF.
              COMPUTE P1Value = P1Value + CardToPick
              DISPLAY P1Value
              IF P2State = "1" THEN
                 PERFORM Player2Turn
              END-IF.
           Player2Turn.
              PERFORM CardsCalculation.
              IF CardToPick = "0" OR "1" THEN
                 ADD 10 TO CardToPick
              END-IF.
              COMPUTE P2Value = P2Value + CardToPick 
              DISPLAY P2Value
              IF P3State = "1" THEN
                 PERFORM Player3Turn
              END-IF.
           Player3Turn.
              PERFORM CardsCalculation.
              IF CardToPick = "0" OR "1" THEN
                 MOVE 10 TO CardToPick
              END-IF.
              COMPUTE P3Value = P3Value + CardToPick
              DISPLAY P3Value            
              IF P4State = "1" THEN
                 PERFORM Player4Turn
              END-IF.
           Player4Turn.
              PERFORM CardsCalculation.
              IF CardToPick = "0" OR "1" THEN
                 MOVE 10 TO CardToPick
              END-IF.
              COMPUTE P4Value = P4Value + CardToPick
              DISPLAY P4Value
           GOBACK.
       end program V2.
