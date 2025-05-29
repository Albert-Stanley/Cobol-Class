      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO09.
       AUTHOR. ALBERT.
       DATE-WRITTEN. 09-05-2025.
       DATE-COMPILED.
       REMARKS. CALCULA PROXIMOS PARES E IMPARES.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-CABECALHO.
          05 FILLER PIC X(30) VALUE "AUTOR: ALUNO COBOL".
          05 FILLER PIC X(20) VALUE "DATA: 09-05-2025".

       01 WS-ENTRADA-DADOS.
          05 WS-NUMERO-ENTRADA    PIC 9(05).
          05 WS-OPCAO-CONTINUAR   PIC X(01).
             88 OPCAO-VALIDA      VALUES ARE "S", "s", "N", "n".
             88 QUER-CONTINUAR    VALUES ARE "S", "s".
             88 QUER-ENCERRAR     VALUES ARE "N", "n".

       01 WS-CALCULOS.
          05 WS-NUMERO-CALCULO    PIC 9(05).
          05 WS-CONTADOR          PIC 9(01).
          05 WS-QUOCIENTE         PIC 9(05).
          05 WS-RESTO             PIC 9(01).

       01 WS-TABELA-PARES.
          05 WS-PAR OCCURS 5 TIMES PIC 9(05).

       01 WS-TABELA-IMPARES.
          05 WS-IMPAR OCCURS 5 TIMES PIC 9(05).

       01 WS-INDICE                PIC 9(01).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "PROGRAMA: EXERCICIO09".
           DISPLAY WS-CABECALHO.
           DISPLAY "--------------------------------------------------".

           PERFORM PROCESSAR-NUMEROS UNTIL QUER-ENCERRAR.

           DISPLAY "--------------------------------------------------".
           DISPLAY "PROGRAMA ENCERRADO.".
           STOP RUN.

       PROCESSAR-NUMEROS.
           DISPLAY " ".
           DISPLAY "DIGITE UM NUMERO INTEIRO:" WITH NO ADVANCING.
           ACCEPT WS-NUMERO-ENTRADA.

           PERFORM CALCULAR-PROXIMOS-PARES.
           PERFORM CALCULAR-PROXIMOS-IMPARES.

           PERFORM MOSTRAR-PARES.
           PERFORM MOSTRAR-IMPARES-DECRESCENTE.

           PERFORM PERGUNTAR-CONTINUAR.

       CALCULAR-PROXIMOS-PARES.
           MOVE WS-NUMERO-ENTRADA TO WS-NUMERO-CALCULO.
           MOVE 0 TO WS-CONTADOR.
           PERFORM UNTIL WS-CONTADOR EQUAL 5
               ADD 1 TO WS-NUMERO-CALCULO
               DIVIDE WS-NUMERO-CALCULO BY 2 GIVING WS-QUOCIENTE
                                       REMAINDER WS-RESTO
               IF WS-RESTO EQUAL 0 THEN
                   ADD 1 TO WS-CONTADOR
                   MOVE WS-NUMERO-CALCULO TO WS-PAR(WS-CONTADOR)
               END-IF
           END-PERFORM.

       CALCULAR-PROXIMOS-IMPARES.
           MOVE WS-NUMERO-ENTRADA TO WS-NUMERO-CALCULO.
           MOVE 0 TO WS-CONTADOR.
           PERFORM UNTIL WS-CONTADOR EQUAL 5
               ADD 1 TO WS-NUMERO-CALCULO
               DIVIDE WS-NUMERO-CALCULO BY 2 GIVING WS-QUOCIENTE
                                       REMAINDER WS-RESTO
               IF WS-RESTO NOT EQUAL 0 THEN
                   ADD 1 TO WS-CONTADOR
                   MOVE WS-NUMERO-CALCULO TO WS-IMPAR(WS-CONTADOR)
               END-IF
           END-PERFORM.

       MOSTRAR-PARES.
           DISPLAY " ".
           DISPLAY "PROXIMOS 5 NUMEROS PARES (CRESCENTE):".
           PERFORM VARYING WS-INDICE FROM 1 BY 1
               UNTIL WS-INDICE > 5
               DISPLAY "PAR " WS-INDICE ": " WS-PAR(WS-INDICE)
           END-PERFORM.

       MOSTRAR-IMPARES-DECRESCENTE.
           DISPLAY " ".
           DISPLAY "PROXIMOS 5 NUMEROS IMPARES (DECRESCENTE):".
           PERFORM VARYING WS-INDICE FROM 5 BY -1
               UNTIL WS-INDICE < 1
               DISPLAY "IMPAR (POS " WS-INDICE "): " WS-IMPAR(WS-INDICE)
           END-PERFORM.

       PERGUNTAR-CONTINUAR.
           DISPLAY " ".
           MOVE SPACES TO WS-OPCAO-CONTINUAR.
           PERFORM UNTIL OPCAO-VALIDA
               DISPLAY "DESEJA INSERIR UM NOVO NUMERO? (S/N):"
                       WITH NO ADVANCING
               ACCEPT WS-OPCAO-CONTINUAR
               IF NOT OPCAO-VALIDA THEN
                  DISPLAY "OPCAO INVALIDA. DIGITE S OU N."
               END-IF
           END-PERFORM.
