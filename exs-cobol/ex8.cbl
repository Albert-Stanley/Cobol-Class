      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
             IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO08.
       AUTHOR. ALBERT.
       DATE-WRITTEN. 09-MAIO-2025.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DISCIPLINAS.
          05 DISCIPLINA-TABELA OCCURS 6 TIMES.
             10 NOME-DISCIPLINA PIC X(30).
             10 NOTA-P1       PIC 9(3)V99.
             10 NOTA-P2       PIC 9(3)V99.
             10 NOTA-ATIVIDADE PIC 9(3)V99.
       01 IDX           PIC 9(01) VALUE 1.

       PROCEDURE DIVISION.

       MOVE "Lógica de Programação" TO NOME-DISCIPLINA (1)
       MOVE "Estruturas de Dados" TO NOME-DISCIPLINA (2)
       MOVE "Banco de Dados" TO NOME-DISCIPLINA (3)
       MOVE "Desenvolvimento Web" TO NOME-DISCIPLINA (4)
       MOVE "Sistemas Operacionais" TO NOME-DISCIPLINA (5)
       MOVE "Redes de Computadores" TO NOME-DISCIPLINA (6)

       MOVE 7.5 TO NOTA-P1 (1)
       MOVE 8.0 TO NOTA-P2 (1)
       MOVE 9.0 TO NOTA-ATIVIDADE (1)

       MOVE 6.5 TO NOTA-P1 (2)
       MOVE 7.0 TO NOTA-P2 (2)
       MOVE 8.5 TO NOTA-ATIVIDADE (2)

       MOVE 9.0 TO NOTA-P1 (3)
       MOVE 9.5 TO NOTA-P2 (3)
       MOVE 10.0 TO NOTA-ATIVIDADE (3)

       MOVE 8.0 TO NOTA-P1 (4)
       MOVE 7.5 TO NOTA-P2 (4)
       MOVE 8.5 TO NOTA-ATIVIDADE (4)

       MOVE 7.0 TO NOTA-P1 (5)
       MOVE 7.5 TO NOTA-P2 (5)
       MOVE 6.0 TO NOTA-ATIVIDADE (5)

       MOVE 9.5 TO NOTA-P1 (6)
       MOVE 9.0 TO NOTA-P2 (6)
       MOVE 8.5 TO NOTA-ATIVIDADE (6)

       DISPLAY "Notas do Aluno em 6 Disciplinas:".

       PERFORM EXIBIR-NOTAS.

       STOP RUN.

       EXIBIR-NOTAS.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 6
               DISPLAY "Disciplina: " NOME-DISCIPLINA (IDX)
               DISPLAY "Nota P1: " NOTA-P1 (IDX)
               DISPLAY "Nota P2: " NOTA-P2 (IDX)
               DISPLAY "Nota Atividade: " NOTA-ATIVIDADE (IDX)
               DISPLAY "-------------------------------"
           END-PERFORM.
