.org #0000h             ; Code start

.code
	xor r0, r0, r0
	ldl r1, #90h ; PortData_ADDR
	ldh r1, #00h
	ldh r2, #90h
	ldl r2, #01h ; PortConfig_ADDR
	ldh r3, #90h
	ldl r3, #02h ; PortEnable_ADDR
	ldh r4, #90h
	ldl r4, #03h ;irqEnable
	
	ldh r5, #01h
	ldl r5, #23h ; r5 <= 0123h
	
	ldl r6, #FFh
	ldh r6, #FFh 
	
	; teste tudo entrada
	st r6, r2, r0 ; PortConfig <= FFFF (entrada)
	st r6, r4, r0 ; irqEnable <= FFFF
	st r6, r3, r0 ; PortEnable <= FFFF
	ld r5, r1, r0 ; r5 <= PortData
	add r7, r1, r2
	halt
.endcode