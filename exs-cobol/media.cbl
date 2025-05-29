       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEDIA.
       AUTHOR. ALBERT.
       REMARKS. CALCULO-DA-MEDIA.


       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.


       DATA DIVISION.
       WORKING-STORAGE SECTION.

           01 NOTA-A PIC S9(2)V9(2) VALUE ZERO.
           01 NOTA-B PIC S9(2)V9(2) VALUE ZERO.
           01 SOMA   PIC S9(2)V9(2) VALUE ZERO.
           01 MEDIA  PIC S9(2)V9(2) VALUE ZERO.
           01 VALIDA PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       INICIO.

           MOVE 'N' TO VALIDA.
           PERFORM UNTIL VALIDA = 'S'
               DISPLAY "DIGITE SUA MEDIA DA P1 ( 0 A 10 ): "
               ACCEPT NOTA-A
               IF NOTA-A >= 0 AND NOTA-A <= 10
                   MOVE 'S' TO VALIDA
               ELSE
                   DISPLAY "VALOR INVALIDO. TENTE NOVAMENTE"
           END-PERFORM.

           MOVE 'N' TO VALIDA.
           PERFORM UNTIL VALIDA = 'S'
               DISPLAY "DIGITE SUA MEDIA DA P2 ( 0 A 10 ): "
               ACCEPT NOTA-B
               IF NOTA-B >= 0 AND NOTA-B <= 10
                   MOVE 'S' TO VALIDA
               ELSE
                   DISPLAY "VALOR INVALIDO. TENTE NOVAMENTE"
           END-PERFORM.


           ADD NOTA-A NOTA-B GIVING SOMA.
           DISPLAY "A SOMA DAS DUAS PROVAS E: " SOMA.

           DIVIDE SOMA BY 2 GIVING MEDIA.
           DISPLAY "A SUA MEDIA E: " MEDIA.

           IF MEDIA >= 6
               DISPLAY "APROVADO !"
           ELSE
               DISPLAY "REPROVADO !".

       STOP RUN.
