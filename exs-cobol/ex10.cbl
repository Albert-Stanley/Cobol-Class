      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROXIMOS-PRIMOS.
       AUTHOR. ALBERT.
       DATE-WRITTEN. 09-MAIO-2025.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 NUMERO-INICIAL           PIC 9(5).
       01 RESPOSTA-USUARIO         PIC X VALUE SPACE.
       01 NUMERO-TESTE             PIC 9(5).
       01 DIVISOR                  PIC 9(5).
       01 RESTO                    PIC 9(5).
       01 E-PRIMO                  PIC X VALUE 'N'.
       01 QUANTOS-ENCONTRADOS      PIC 9(1) VALUE 0.
       01 I                        PIC 9(1).
       01 PRIMOS-ENCONTRADOS.
          05 PRIMO-TAB OCCURS 5 TIMES.
             10 VALOR-PRIMO        PIC 9(5).

       PROCEDURE DIVISION.
       INICIO.
           PERFORM APLICACAO
           STOP RUN.

       APLICACAO.
           PERFORM UNTIL RESPOSTA-USUARIO = 'N'
               DISPLAY "Digite um número: "
               ACCEPT NUMERO-INICIAL
               ADD 1 TO NUMERO-INICIAL
               MOVE 0 TO QUANTOS-ENCONTRADOS

               PERFORM BUSCAR-PRIMOS

               DISPLAY " "
               DISPLAY "Próximos 5 números primos (crescente):"
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
                   DISPLAY VALOR-PRIMO(I)
               END-PERFORM

               DISPLAY " "
               DISPLAY "Próximos 5 números primos (decrescente):"
               PERFORM VARYING I FROM 5 BY -1 UNTIL I < 1
                   DISPLAY VALOR-PRIMO(I)
               END-PERFORM

               DISPLAY "Deseja inserir outro número? (S/N): "
               ACCEPT RESPOSTA-USUARIO
          MOVE FUNCTION UPPER-CASE(RESPOSTA-USUARIO) TO RESPOSTA-USUARIO
           END-PERFORM.

       BUSCAR-PRIMOS.
           MOVE NUMERO-INICIAL TO NUMERO-TESTE
           PERFORM UNTIL QUANTOS-ENCONTRADOS = 5
               PERFORM VERIFICAR-PRIMO
               IF E-PRIMO = 'S'
                   ADD 1 TO QUANTOS-ENCONTRADOS
                   MOVE NUMERO-TESTE TO VALOR-PRIMO(QUANTOS-ENCONTRADOS)
               END-IF
               ADD 1 TO NUMERO-TESTE
           END-PERFORM.

       VERIFICAR-PRIMO.
           MOVE 'S' TO E-PRIMO
           MOVE 2 TO DIVISOR
           PERFORM UNTIL DIVISOR * DIVISOR > NUMERO-TESTE
             DIVIDE NUMERO-TESTE BY DIVISOR GIVING RESTO REMAINDER RESTO
               IF RESTO = 0
                   MOVE 'N' TO E-PRIMO
                   EXIT PERFORM
               END-IF
               ADD 1 TO DIVISOR
           END-PERFORM.
