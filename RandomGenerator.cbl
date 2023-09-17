       IDENTIFICATION DIVISION.
       program-id. RandomGenerator.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 NumberOfCharacterChosen PIC 9(15) VALUE 0.
       01 Created PIC 9(15).
       77 UserResponse PIC X(12).
       01 Current-Time PIC 9(15).
       01 TestValue PIC 9(15).
       linkage section.

       procedure division.
           
           STOP RUN.
           goback.
       end program RandomGenerator.
       *>Début 14/09/2023/Pause a partir du 15/09/2023
