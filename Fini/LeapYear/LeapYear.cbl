       program-id. LeapYear.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 Year PIC 9(4) VALUE 0.
       01 YearToDisplay PIC ****.
       01 YearBy4 PIC 9(4).
       01 YearBy100 PIC 9(4).
       01 YearBy400 PIC 9(4).
       linkage section.

       procedure division.
           DISPLAY "Entrez année positive (4 chiffres max): "
           ACCEPT Year.
           MOVE Year TO YearToDisplay.
           IF Year <= 0 THEN
              DISPLAY YearToDisplay " est une année bissextile."
              STOP RUN
           END-IF.
           COMPUTE YearBy4 = FUNCTION MOD (Year 4).
           COMPUTE YearBy100 = FUNCTION MOD (Year 100).
           COMPUTE YearBy400 = FUNCTION MOD (Year 400).

           IF (YearBy4 = 0 AND YearBy100 NOT = 0) OR YearBy400 = 0 THEN
              DISPLAY YearToDisplay " est une année bissextile."
           ELSE 
              DISPLAY YearToDisplay " n'est pas une année bissextile."
           END-IF.
           goback.
       end program LeapYear.
