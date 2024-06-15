       IDENTIFICATION DIVISION.
       PROGRAM-ID. IsogramCheck.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Word PIC X(150).
       01 IsAnIsogram PIC 9 VALUE 1.
       01 Letters-List PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
       01 Letters-Index  PIC 9(3).
       01 Letters-Count PIC 9(3).
       01 Char PIC X.

       PROCEDURE DIVISION.
           DISPLAY "Entrez une phrase :".
           ACCEPT Word.
           PERFORM ISOGRAM.
           IF IsAnIsogram = 1
               DISPLAY "C'est un isogramme."
           ELSE
               DISPLAY "Ce n'est pas un isogramme."
           END-IF.
           STOP RUN.

       ISOGRAM.
           MOVE FUNCTION UPPER-CASE (Word) TO Word
           PERFORM VARYING Letters-Index FROM 1 BY 1
            UNTIL Letters-Index > 26 OR IsAnIsogram = 0
                 MOVE 0 TO Letters-Count
                 MOVE Letters-List(Letters-Index:1) TO Char
                 INSPECT Word TALLYING Letters-Count 
                    FOR ALL Char
                 IF Letters-Count > 1
                    MOVE 0 TO IsAnIsogram
                 END-IF
           END-PERFORM.
           MOVE 1 TO Letters-Index.
       end program IsogramCheck.
