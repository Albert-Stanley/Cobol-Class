       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO13.
       AUTHOR. ALBERT.
       DATE-WRITTEN. 23-05-2025.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 NOTA-UM        PIC 99V9.
       77 NOTA-DOIS      PIC 99V9.
       77 NOTA-TRES      PIC 99V9.
       77 MEDIA          PIC 99V9.
       77 RESULTADO      PIC X(10).
       77 CONTINUAR      PIC X VALUE "S".

       PROCEDURE DIVISION.
      * Programa para calcular a média de 3 notas
      * e repetir o processo conforme a vontade do usuário.
      * Entrada: NOTA-UM, NOTA-DOIS, NOTA-TRES
      * Saída: MEDIA e RESULTADO ("APROVADO" ou "REPROVADO")

           PERFORM AVALIAR-NOTAS
               UNTIL CONTINUAR NOT = "S"

           STOP RUN.

       AVALIAR-NOTAS.
           DISPLAY "Digite a primeira nota: ".
           ACCEPT NOTA-UM.

           DISPLAY "Digite a segunda nota: ".
           ACCEPT NOTA-DOIS.

           DISPLAY "Digite a terceira nota: ".
           ACCEPT NOTA-TRES.

           COMPUTE MEDIA = (NOTA-UM + NOTA-DOIS + NOTA-TRES) / 3.

           IF MEDIA >= 7.0 THEN
               MOVE "APROVADO" TO RESULTADO
           ELSE
               MOVE "REPROVADO" TO RESULTADO
           END-IF.

           DISPLAY "Media: " MEDIA.
           DISPLAY "Resultado: " RESULTADO.

           DISPLAY "Deseja calcular outra media? (S/N)".
           ACCEPT CONTINUAR.

           MOVE FUNCTION UPPER-CASE(CONTINUAR) TO CONTINUAR.
