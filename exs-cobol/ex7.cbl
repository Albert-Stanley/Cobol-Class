      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO07.
       AUTHOR. ALBERT.
       DATE-WRITTEN. 09-MAIO-2025.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DISCIPLINAS.
          05 DISCIPLINA-TABELA OCCURS 6 TIMES.
             10 NOME-DISCIPLINA PIC X(30).
       01 IDX           PIC 9(01) VALUE 1.

       PROCEDURE DIVISION.

       MOVE "1- Lógica de Programação" TO NOME-DISCIPLINA (1)
       MOVE "2- Estruturas de Dados" TO NOME-DISCIPLINA (2)
       MOVE "3- Banco de Dados" TO NOME-DISCIPLINA (3)
       MOVE "4- Desenvolvimento Web" TO NOME-DISCIPLINA (4)
       MOVE "5- Sistemas Operacionais" TO NOME-DISCIPLINA (5)
       MOVE "6- Redes de Computadores" TO NOME-DISCIPLINA (6)

       DISPLAY "Disciplinas do Curso de ADS:".

       PERFORM EXIBIR-DISCIPLINAS.

       STOP RUN.

       EXIBIR-DISCIPLINAS.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 6
               DISPLAY NOME-DISCIPLINA (IDX)
           END-PERFORM.
