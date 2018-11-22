#!/usr/bin/env python
# Author Dario Clavijo 2018
# GPLv2
# MIPS CPU instruction set Emulator
# WIP

memory = []

REGBITS = 0xFFFFFFFF #32 bit Regs
regs = [31] 
PC = 0
HI = 0
LO = 0
EPC = 0
DIV0 = 0
OF = 0
CARRY = 0
Cause_Reg = 0

"""
0 INT Interrupt
4 ADDRL Load from an illegal address
5 ADDRS Store to an illegal address
6 IBUS Bus error on instruction fetch
7 DBUS Bus error on data reference
8 SYSCALL syscall instruction executed
9 BKPT break instruction executed
10 RI Reserved instruction
12 OVF Arithmetic overflow
"""


def twos_complement(input_value, num_bits):
	'''Calculates a two's complement integer from the given input value's bits'''
	mask = 2**(num_bits - 1)
	return -(input_value & mask) + (input_value & ~mask)


def sint(value):
	mask = 2**(32)
	if (value & mask >> 31)  == 1:
		return (value + 1) *-1
	return value

def decode_and_execute(instruction):
	opcode = (instruction[0] & 0b11111100) >> 2
	
	if opcode == 0x0: # R
                # 000000ssssstttttdddddSSSSSfffff
		rs = (instruction[0] & 0b00000011)<<8 + ((instruction[1] & 0b11100000) >> 5)
		rt = instruction[1] & 0b00011111
		rd = (instruction[2] & 0b11111000) >> 3
		shamt = (insctuction[2] & 0b00000111) << 8 + (instruction[3] & 0b11000000) >> 6
		funct = (instruction[3] & 0b11111100) >> 2

		if funct == 0x00: #NOP
			PC = PC + 4
		else if funct == 0x08: #JR
			PC = regs[rs]
		else if funct == 0x09: #JALR
			regs[rd] = PC
			PC = regs[rs]
		else if funct == 0x10: #MFHI
			regs[rd] = HI
			PC = PC + 4
		else if funct == 0x12: #MFLO
			regs[rd] = LO
			PC = PC + 4
		else if funct ==0x18: #MUL WIP
	                HILO = sint(regs[rs]) * sint(regs[rt])
			HI = HILO >> REGBITS
			LO = (HILO << REGBITS) >> REGBITS
                        PC = PC + 4
		else if funct == 0x19: #MULU
			HILO = regs[rs] * regs[rt]
                        HI = HILO >> REGBITS
                        LO = (HILO << REGBITS) >> REGBITS
			PC = PC + 4
                else if funct == 0x1A: #DIV 
                        try:
                                LO = sint(regs[rs]) / sint(regs[rt])
                                HI = sint(regs[rs]) % sint(regs[rt])
                                PC = PC + 4
                        except:
                                EPC = PC
                                DIV0 = 1
                                Cause_Reg = 12
                                PC = 0x80000180
                else if funct == 0x1B: #DIVU
                        try:
                                LO = regs[rs] / regs[rt]
                                HI = regs[rs] % regs[rt]
                                PC = PC + 4
                        except:
                                EPC = PC
                                DIV0 = 1
                                Cause_Reg = 12
                                PC = 0x80000180
		else if funct == 0x20: #ADD
			regs[rd] = sint(regs[rs]) + sint(regs[rt])
			OF = (sint(regs[rd]) >= REGBITS)
			if OF == 1:
                                Cause_Reg = 0b1100
                        else:
                                PC = PC + 4
		else if funct == 0x21: #ADDU 
			regs[rd] = sint(regs[rs]) + sint(regs[rt])
			OF = (sint(regs[rd]) >= REGBITS)
			if OF == 1:
                                Cause_Reg = 0b1100
                        else:
                                PC = PC + 4
		else if funct == 0x22: #SUB
			regs[rd] = sint(regs[rs]) - sint(regs[rt])
			OF = (sint(regs[rd]) >= REGBITS)
			if OF == 1:
                                Cause_Reg = 0b1100
                        else:
                                PC = PC + 4	
		else if funct == 0x23: #SUBU
			regs[rd] = regs[rs] - regs[rt]
			OF = (sint(regs[rd]) >= REGBITS)
			if OF == 1:
                                Cause_Reg = 0b1100
                        else:
                                PC = PC + 4	
		else if funct == 0x24: #AND
			regs[rd] = regs[rs] & regs[rt]
			PC = PC + 4
		else if funct == 0x25: #OR
			regs[rd] = regs[rs] | regs[rt]
                        PC = PC + 4
		else if funct == 0x26: #XOR
			regs[rd] = regs[rs] ^ regs[rt]
			PC = PC + 4
		else if funct == 0x27: #NOR
			regs[rd] = (regs[rs] | regs[rt]) ^ REGBITS
			PC = PC + 4
		else if funct == 0x2A: #SLT		
			if sint(regs[rs]) < sint(regs[rt]):
				regs[rd] = sint(1)
			else
				regs[rd] = sint(0)
			PC = PC + 4
		else if funct == 0x2B: #SLTU		
			if regs[rs] < regs[rt]:
                                regs[rd] = 1
                        else
                                regs[rd] = 0
			PC = PC + 4
		else:
			EPC = PC
                        Cause_Reg = 10
                        PC = 0x80000180

		print "R opcode,rt,rs,rd,shamt,funct",opcode,rs,rt,rd,shamt,funct

	else if opcode > 3: # I
		#000000ssssstttttiiiiiiiiiiiiiiii
		rs = (instruction[0] & 0b00000011) << 8 + ((instruction[1] & 0b11100000) >> 5)
	        rt = instruction[1] & 0b00011111
        	inmediate = instruction[2] << 8 + instruction[3]

		if opcode == 0x01: # BLTZ
			if regs[rs] < 0:
				PC = PC + 4 + 4 * (inmmediate & 0xFFFF)
			else:
				PC = PC + 4

		if opcode == 0x08: #ADDI
			regs[rt] = sint(regs[rs]) + sint(inmediate & 0xFFFF)
			OF = (sint(regs[rt]) >= REGBITS)
			if OF == 1:
				EPC = PC
				Cause_Reg = 0b1100
				PC = 0x80000180
			else:
				PC = PC + 4
		else if opcode == 0x09: #ADDIU
			regs[rt] = regs[rs] + inmediate & 0xFFFF
			OF = (regs[rt] >= REGBITS)
                        if OF == 1:
                                Cause_Reg = 0b1100
			else:
				PC = PC + 4
		else if opcode == 0x0C: #ANDI 
			regs[rt] = sint(regs[rs]) & inmediate & 0xFFFF 
			PC = PC + 4
		else if opcode == 0x04: #BEQ
			if regs[rt] = regs[rt] 
				PC += 4 + 4 * (inmediate & 0xFFFF)
			else:
				PC = PC + 4
		else if opcode == 0x05: #BNEQ
			if regs[rt] != regs[rt] 
                                PC += 4 + 4 * (inmediate & 0xFFFF)
                        else:
                                PC = PC + 4
		else if opcode == 0x06: #BLEZ
			if regs[rs] <= 0:
				PC = PC + 4 + 4 * (inmediate & 0xFFFF)
			else:
				PC = PC + 4
		else if opcode == 0x07: #BGTZ
			if regs[rs] > 0:
				PC = PC + 4 + 4 * (inmediate & 0xFFFF)
			else:
				PC = PC + 4
		else if opcode == 0x10: # MFEPC/MFCO
			if rt = 0b01110:
				regs[rd] = EPC
			if rt = 0b01101:
				regs[rd] = Cause_Reg
			PC = PC + 4
		else if opcode == 0x0A: #SLTI
			if regs[rs] < sint(inmediate & 0xFFFF):
				regs[rs] = sign(1)
			else:
				regs[rs] = sign(0)
			PC = PC + 4
		else if opcode == 0x0D: #ORI
			regs[rd] = regs[rs] | (inmediate & 0xFFFF)
			PC = PC + 4
		else if opcode == 0x20:	# LB WIP
			regs[rt] = memory[rs+(inmediate & 0xFFFF):rs+(inmediate & 0xFFFF)]
			PC = PC + 4
		else if opcode == 0x23: # LW WIP
			try:
				regs[rt] = memory[rs+(inmediate & 0xFFFF):rs+(inmediate & 0xFFFF)+0]
				regs[rt] += memory[rs+(inmediate & 0xFFFF):rs+(inmediate & 0xFFFF)+1] << 8 #*256 #2**8
				regs[rt] += memory[rs+(inmediate & 0xFFFF):rs+(inmediate & 0xFFFF)+2] << 16 #*65535 #2**16
				regs[rt] += memory[rs+(inmediate & 0xFFFF):rs+(inmediate & 0xFFFF)+3] << 24 #*16777216 #2**24
				PC = PC + 4
			except:
				EPC = PC
				Cause_Reg = 0b100
				PC = 0x80000180

		else if opcode == 0x28: # LB WIP
			memory[rs+(inmediate & 0xFFFF)] = regs[rt] & 0xFF
			PC = PC + 4 
		else if opcode == 0x2b: # SW WIP
			try:
				memory[rs+(inmediate & 0xFFFF)+0] = (regs[rt] & 0b00000000000000000000000011111111)
				memory[rs+(inmediate & 0xFFFF)+1] = (regs[rt] & 0b00000000000000001111111100000000) >> 8
				memory[rs+(inmediate & 0xFFFF)+2] = (regs[rt] & 0b00000000111111110000000000000000) >> 16
				memory[rs+(inmediate & 0xFFFF)+3] = (regs[rt] & 0b11111111000000000000000000000000) >> 24
				PC = PC + 4
			except:	
				EPC = PC
				Cause_Reg = 0b101
				PC = 0x80000180
		else:
                	EPC = PC
	                Cause_Reg = 0b1010
        	        PC = 0x80000180

		print "I opcode,rt,rs,inmediate",opcode,rs,rt,inmediate		

	else if opcode == 0x02 and opcode == 0x03: # J
		#000000aaaaaaaaaaaaaaaaaaaaaaaaaa	
		address = (instruction[0] & 0b11000000 >> 6)*256 + instruction[1]
		if opcode == 0x2: #J
			PC = address
		if opcode == 0x3: #JAL
			regs[31] = PC
			PC = address
		print "J opcode,address",opcode,address,

	else:
		EPC = PC
		Cause_Reg = 0b1010
		PC = 0x80000180

	print "regs",regs
	print "EPC,PC,ret,OF,CARRY,DIV0,Cause_Reg",EPC,PC,ret,OF,CARRY,DIV0,Cause_Reg

def load_ram(fname):
	fp = open(fname)
	memory = fp.read()
	fp.close()

def save_ram(fname):
	fp = open(sys.argv[1])
	fp.write(memory)
	fp.close()


def execute():
	ramfile = sys.argv[1]
	load_ram(ramfile)
	while True:
		instruction = memory[PC:PC+4]
		decode_and_execute(instruction)
	save_ram(ramfile)

execute()
