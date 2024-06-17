       IDENTIFICATION DIVISION.
       PROGRAM-ID. PANGRAM.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Phrase PIC X(350).
       01  IsAnIsogram PIC 9 VALUE 1.
       01  LetterCount PIC 9(3).
       01  CurrentLetter PIC X.
       01  CharactersList PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
       01  I PIC 9(3).

       PROCEDURE DIVISION.
           DISPLAY "Entrez une phrase : ".
           ACCEPT Phrase.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 26 OR IsAnIsogram = 0
               MOVE 0 TO LetterCount
               MOVE FUNCTION LOWER-CASE(CharactersList(I:1)) 
               TO CurrentLetter
               INSPECT FUNCTION LOWER-CASE(Phrase) 
               TALLYING LetterCount FOR ALL CurrentLetter
               IF LetterCount = 0 THEN
                   MOVE 0 TO IsAnIsogram
               END-IF
           END-PERFORM.
           
           IF IsAnIsogram = 1 THEN
               DISPLAY "La phrase est un pangramme."
           ELSE
               DISPLAY "La phrase n'est pas un pangramme."
           END-IF.

           STOP RUN.

       END PROGRAM PANGRAM.
