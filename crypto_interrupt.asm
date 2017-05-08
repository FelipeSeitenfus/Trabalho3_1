; r9 - r15 => bitmasks to crypto operations
; r0 => zero 
; r1 => PortData (A or B)
; r2 => PortConfig (A or B)
; r3 => random number (counter) after PortEnable ativation
; r4 - r6 => temporarios
; r7 => retorno para as funcoes
; r8 => escreve/le das portas


.org #0000h             ; Code start

.code
; ----------------------- INICIALIZAÇÕES DAS PORTAS E ENDEREÇOS -------------------
boot:	
	ldh r0, #01h
	ldl r0, #AEh ;
	ldsp r0 ; SP <= 01AEh
	xor r0, r0, r0 ; r0 <= 0
	xor r7, r7, r7 ; retorno para as funções
	
	; PortA addresses
	ldh r1, #80h
	ldl r1, #00h ; PortDataA_ADDR
	ldh r2, #80h
	ldl r2, #01h ; PortConfigA_ADDR
	ldh r3, #80h
	ldl r3, #02h ; PortEnableA_ADDR
	
	; PortB addresses
	ldh r4, #90h
	ldl r4, #00h ; PortDataB_ADDR
	ldh r5, #90h
	ldl r5, #01h ; PortConfigB_ADDR
	ldh r6, #90h
	ldl r6, #02h ; PortEnableB_ADDR
	ldh r7, #90h
	ldl r7, #03h ; IRQ_ENABLE_ADDR
	
	ldh r15, #address_PortData_A
	ldl r15, #address_PortData_A
	st r1, r15, r0 ; address_PortData_A
	
	ldh r15, #address_PortData_B
	ldl r15, #address_PortData_B
	st r4, r15, r0 ; address_PortData_B
	
	ldh r15, #address_PortConfig_A
	ldl r15, #address_PortConfig_A
	st r2, r15, r0 ; address_PortData_A
	
	ldh r15, #address_PortConfig_B
	ldl r15, #address_PortConfig_B
	st r5, r15, r0 ; address_PortData_B
	
	ldh r15, #60h
	ldl r15, #FFh ; PortConfig <= 0110000011111111
	st r15, r2, r0 ; PortConfigA <= 0110000011111111
	
	ldh r15, #74h
	ldl r15, #FFh ; PortConfig <= 0111010011111111
	st r15, r5, r0 ; PortConfigB <= 0111010011111111
	
	st r0, r1, r0 ; PortData_A <= 0000h
	st r0, r4, r0 ; PortData_B <= 0000h
	
	ldh r15, #24h ; 00100100
	ldl r15, #00h ;
	st r15, r7, r0 ; irqEnable_PortB
	
	; enable PortA and PortB
	ldh r15, #98h
	ldl r15, #FFh
	st r15, r3, r0 ; PortEnable_A <= 1101100011111111
	ldh r15, #FCh
	st r15, r6, r0 ; PortEnable_B <= 1111110011111111

	; RANDOM NUMBER INICIALIZATION
	ldh r15, #random_number
	ldl r15, #random_number
	xor r3, r3, r3 ; r3 <= 0 ; random number
	st r3, r15, r0 ; random_number
	
	
	
; ---------------------------------------- INICIO CRYPTO1 ---------------------------------------------	

inicio_crypto1:
	; carregar endereços dos regs 
	ldh r1, #address_PortData_A
	ldl r1, #address_PortData_A
	ld r1, r1, r0 ; r1 <= address_PortData_A
	
	ldh r2, #address_PortConfig_A
	ldl r2, #address_PortConfig_A
	ld r2, r2, r0 ; r2 <= address_PortConfig_A
	
	ldh r3, #random_number
	ldl r3, #random_number
	ld r3, r3, r0 ; r3 <= random_number
	jsrd #testa_crypto
	jsrd #calcula_chave ; chave em r7 ou em #chave
	
	xor r6, r6, r6 ; r6 <= contador de caracteres
	
	jsrd #le_caractere_crypto
	and r4, r15, r4 ; verifica fim da mensagem
	jmpzd #move_data_up_crypto1
	jmpd #guarda_caractere_crypto1
	
move_data_up_crypto1:
	jsrd #move_data_up_crypto

le_caractere2_crypto1: ; r8 <= caractere par
	ld r4, r1, r0 ; r4 <= PortData_A
	and r5, r14, r4
	JMPZD #le_caractere2_crypto1 ; pooling enquanto o caractere nao esta pronto
	or r5, r4, r12 ;
	st r5, r1, r0 ; pulso em ack
	xor r5, r5, r12 ;
	st r5, r1, r0 ; limpa o ack
	and r5, r4, r10 ; limpa a parte alta de PortDataA
	xor r5, r7, r5 ; descriptografa a mensagem
arruma_data_crypto1: ; parte baixa da mensagem pronta em r5
	or r8, r8, r5 ; mensagem pronta para gravar em r8
guarda_caractere_crypto1:
	ldh r5, #msg_c1 ;
	ldl r5, #msg_c1 ; r5 <= ponteiro para a variavel
	st r8, r5, r6 ; grava na memória
	addi r6, #1 ; r6++
	and r4, r15, r4 ; verifica fim da mensagem
	jmpzd #le_caractere1_crypto1
fim_mensagem_crypto_1: 
	
; ------------------------------------- FIM CRYPTO1 -------------------------------------------------


; ------------------------------------- INICIO CRYPTO2 ----------------------------------------------
inicio_crypto2:
	;carregar endereços dos regs 
	ldh r1, #address_PortData_B
	ldl r1, #address_PortData_B
	ld r1, r1, r0 ; r1 <= address_PortData_B
	
	ldh r2, #address_PortConfig_B
	ldl r2, #address_PortConfig_B
	ld r2, r2, r0 ; r2 <= address_PortConfig_B
	
; ---------------------------------- FUNÇÕES GERAIS DA APLICAÇÃO ----------------------------------	
	
; finds a^b mod q 
; receives r6 as "a"
; receives r3 as "b"
; receives r5 as "q"
; returns the answer in "r7" register
exp_mod:
	push r1
	ldh r4, #00h
	ldl r4, #80h ; bitmask para testes
	
	ldh r7, #00h
	ldl r7, #01h ; resposta <= 1
	addi r4, #00h
loop:
	jmpzd #fim_find_key
	mul r7, r7
	mfl r7 ; r7 <= r7^2
	div r7, r5
	mfh r7 ; r7 <= r7^2 mod q
	and r1, r4, r3
	jmpzd #continue_loop
multiplica:
	mul r7, r6
	mfl r7 ; r7 <= r7 * a
	div r7, r5
	mfh r7 ; r7 <= r7 * a mod q
continue_loop:
	SR0 r4, r4 
	jmpd #loop
fim_find_key:
	;resposta em r7
	pop r1
	rts
	
; r8 is the in and out
; move the lower bits to the higher part	
move_high:
	push r6
	ldl r6, #08h
	ldh r6, #00h
	addi r6, #0
shift: ; shift left de 8 bits 
	jmpzd #continue_move
	sl0 r8, r8
	subi r6, #1
	jmpd #shift
continue_move:
	pop r6
	rts

; verifica o numero aleatorio e garante que ele é menor que 251
; r3 in/out
verifica_num_alet:
		push r5
		ldh r5, #00h
		ldl r5, #FBh
		sub r5, r3, r5
		jmpzd #reinicia_numero
		pop r5
		rts
reinicia_numero:
		ldh r3, #00h
		ldl r3, #00h
		pop r5
		rts
		
bitmask_init:
	ldh r10, #00h
	ldl r10, #FFh ; r10 <= bitmask para data_in(crypto) -- LOW BYTE MASK
	ldh r11, #FFh
	ldl r11, #00h ; r11 <= HIGH BYTE MASK
	ldh r12, #80h
	ldl r12, #00h ; r12 <= bitmask para ack
	ldh r13, #08h
	ldl r13, #00h ; r13 <= bitmask para in/out
	ldh r14, #40h
	ldl r14, #00h ; r14 <= bitmask para data_av 
	ldh r15, #10h
	ldl r15, #00h ; r15 <= bitmask para eom 
	rts
	
; magicNumber retornado em r7 e guardado em magicNumberFromProcessor
calcula_magic_number:
	; prepara para a chamada de exp_mod (parametros)
	ldh r5, #00h
	ldl r5, #FBh ; q <= 251
	ldh r6, #00h
	ldl r6, #06h ; a <= 6
	jsrd #verifica_num_alet
	addi r3, #1 ; incrementa o numero aleatorio
	jsrd #exp_mod ; resposta retornada em r7 => magicNumberFromProcessor
	ldh r4, #magicNumberFromProcessor
	ldl r4, #magicNumberFromProcessor
	st r7, r4, r0 ; magicNumberFromProcessor <= r7
	; magicNumber do processador em r7
	ld r4, r1, r0
	or r4, r4, r13 ; seta o bit para o tristate
	st r4, r1, r0 

	ld r4, r2, r0 ; r4 <= PortConfig
	and r4, r4, r11 ; r4 <= Porta vira saida
	st r4, r2, r0 ; PortA(7:0) => saida
	
	or r5, r7, r13 ; seta o magicNumber e o bit do tristate
	or r5, r5, r12 ; seta ack
	st r5, r1, r0 ; PortData <= MagicNumber + ack + tristate_signal
	xor r5, r5, r12 ; desativa o ack
	st r5, r1, r0 ; desativa o ack
	
	ld r4, r2, r0 ; r4 <= PortConfig
	or r4, r10, r4 ; 
	st r4, r2, r0 ; Port(7:0) => entrada
	xor r4, r4, r4 ;
	st r4, r1, r0 ; desativa o tristate
	rts
	
; pega o magicNumber do crypto e calcula o magicNumber do processador
testa_crypto:
	and r8, r8, r10 ; BITS do magic number do Crypto
	ldh r4, #magicNumberFromCrypto
	ldl r4, #magicNumberFromCrypto
	st r8, r4, r0 ; magicNumberFromCrypto <= magicNumberFromCrypto
	; calcula_magic_number
	jsrd #calcula_magic_number
	rts
	
calcula_chave:
	ldh r4, #magicNumberFromCrypto
	ldl r4, #magicNumberFromCrypto
	ld r6, r4, r0 ; r6 <= MagicNumber do crypto A
	ldh r5, #00h
	ldl r5, #FBh ; r5 <= q (251)
	jsrd #exp_mod ; chave em r7
	ldh r4, #chave
	ldl r4, #chave
	st r7, #chave
	rts
	
le_caractere_crypto:
	ld r4, r1, r0  ; r4 <= PortData_A
	and r5, r14, r4
	JMPZD #le_caractere_crypto ; pooling enquanto o caractere nao esta pronto
	or r5, r4, r12 ;
	st r5, r1, r0 ; pulso em ack
	xor r5, r5, r12 ;
	st r5, r1, r0 ; limpa o ack
	and r8, r4, r10 ; limpa a parte alta de PortDataA
	xor r8, r7, r8 ; descriptografa a mensagem
	ldh r5, #buffer
	ldl r5, #buffer
	st r8, r5, r0 ; buffer <= mensagem descriptografa
	rts	
	
move_data_up_crypto:
	ldh r5, #buffer
	ldl r5, #buffer
	ld r8, r5, r0 ; r8 <= buffer
	jsrd #move_high ; r8 <= caractere (parte alta)
	st r8, r5, r0 ; buffer <= r8 << 8
	rts
.endcode

; Data area (variables)
.data
	address_PortData_A: db #00h
	address_PortData_B: db #00h
	address_PortConfig_A: db #00h
	address_PortConfig_B: db #00h
	buffer: db #00h
	random_number: db #00h
	chave: db #00h
	magicNumberFromProcessor: db #00h
	magicNumberFromCrypto: db #00h
	msg_c1: db #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h
    msg_c2: db #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h
.enddata
