       program-id. FibonnaciSequence.

       environment division.
       configuration section.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA. 
       data division.
       working-storage section.
       
       01 UserResponse PIC X(18).
       01 NbOfNumbersToCreate PIC 999.
       01 NbCreate PIC 999.
       01 NumberCreated PIC 9(3).
       01 PreviousNb PIC 9(3).
       01 2PreviousNumber PIC 9(3).
       01 Array OCCURS 999 TIMES PIC 9(9).
       01 ArIndex PIC 9(3).
       linkage section.

       procedure division.
           MOVE 1 TO Array(1).
           DISPLAY "Nb de chiffre a générer"
           ACCEPT NbOfNumbersToCreate
           PERFORM CalculateSequence
           PERFORM DisplayNumbers
           DISPLAY "fin du calcul"
           goback.
       CalculateSequence.
           PERFORM NbOfNumbersToCreate TIMES
              ADD 1 TO NbCreate
              IF (NbCreate IS EQUAL TO 1)
                 MOVE 1 TO NumberCreated
                 MOVE NumberCreated TO ARRAY(2)
                 MOVE 2PreviousNumber TO Array(NbCreate)
                 MOVE PreviousNb TO Array(NbCreate - 1)
              ELSE
                 COMPUTE NumberCreated = PreviousNb + 2PreviousNumber
                 MOVE NumberCreated TO Array(NbCreate)
                 MOVE PreviousNb TO 2PreviousNumber
                 MOVE NumberCreated TO PreviousNb
              END-IF
           END-PERFORM.
        DisplayNumbers.
           MOVE 1 TO ArIndex
           PERFORM UNTIL ArIndex > NbCreate
              DISPLAY Array(ArIndex)
              ADD 1 TO ArIndex
           END-PERFORM.
       end program FibonnaciSequence.
