#!/usr/bin/env python
# Author Dario Clavijo 2018
# GPLv2
# MIPS CPU instruction set Emulator
# WIP
import struct
import sys
import os
import time

def uint32tobytes(value,big_endian=True):
        if big_endian:
                return struct.pack(">I", value)
        else:
                return struct.pack("<I", value)

def bytestouint32(value,big_endian=True):
        if big_endian:
                return struct.unpack(">I", value)[0]
        else:
                return struct.unpack("<I", value)[0]

def int32tobytes(value,big_endian=True):
        if big_endian:
                return struct.pack(">i", value)
        else:
                return struct.pack("<i", value)

def bytestoint32(value,big_endian=True):
        if big_endian:
                return struct.unpack(">i", value)[0]
        else:
                return struct.unpack("<i", value)[0]
	
memory = []

KERNEL_MODE = 0x80000000
KERNEL_MODE_UTLB = KERNEL_MODE
KERNEL_MODE_OTHER = KERNEL_MODE + 0x80
PROCESOR_RESET = 0xbfc000000 
PROCESOR_RESET_UTLB = PROCESOR_RESET + 0x100
PROCESOR_RESET_OTHER = PROCESOR_RESET + 0x180
"""
Address Description
0x80000000  UTLB miss exception
0x80000080  Other exceptions
0xbfc00000  Processor reset
0xbfc00100  UTLB miss exception, if BEV is set in c0_status
0xbfc00180  Other exceptions, if BEV is set in c0_status
"""

REGBITS = 0xFFFFFFFF #32 bit Regs
regs = [uint32tobytes(0) * 31] 
PC = 0
HI = uint32tobytes(0)
LO = uint32tobytes(0)
EPC = uint32tobytes(0)
DIV0 = 0 # Flag
OF = 0 # Flag
CARRY = 0 # Flag
Cause_Reg = uint32tobytes(0)

"""
Memory maps
0xffffffff  kseg2    
0xc0000000   
0xbfffffff  kseg1    
0xbfc00180          Exception address if BEV set.
0xbfc00100          UTLB exception address if BEV set.
0xbfc00000          Execution begins here after processor reset.
0xa0000000   
0x9fffffff  kseg0    
0x80000080          Exception address if BEV not set.
0x80000000          UTLB exception address if BEV not set.
0x7fffffff  kuseg    
0x00000000   

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

def zero_extend(value):
	tmp = value
	for i in range(0,4-len(value)):	
		tmp = chr(0) + tmp
	return tmp

def sint3232(value):
	return twos_complement(value,32)

def decode_and_execute(instruction):
	opcode = (instruction[0] & 0b11111100) >> 2
	
	if opcode == 0x0: # R
                # OOOOOOssssstttttdddddSSSSSfffff
		rs = (instruction[0] & 0b00000011)<<8 + ((instruction[1] & 0b11100000) >> 5)
		rt = instruction[1] & 0b00011111
		rd = (instruction[2] & 0b11111000) >> 3
		shamt = (insctuction[2] & 0b00000111) << 8 + (instruction[3] & 0b11000000) >> 6
		funct = (instruction[3] & 0b11111100) >> 2

		if funct == 0x00: #SLL R0,0 / NOP
			#regs[rd] = regs[rt] << shamt
			val = regs[rt]
    			for i in range(0,shamt):
        			val <<= 1
        			val |= 0
    			regs[rd] = val & 0xFFFFFFFF
			PC = PC + 4
		else if funct == 0x02: #SRL 
			#regs[rd] = regs[rt] >> shamt
			val = regs[rt]
    			for i in range(0,shamt):
        			val >>= 1
        			val |= 0
    			regs[rd] = val & 0xFFFFFFFF
                        PC = PC + 4
		else if funct == 0x03: #SRA 
			#regs[rd] = (regs[rt] & 0x80000000) + (regs[rt] >> shamt)
			val = regs[rt]
    			s = val & 0x80000000
    			for i in range(0,shamt):
        			val >>= 1
        			val |= s
    			regs[rd] = val & 0xFFFFFFFF
			PC = PC + 4
		else if fimct == 0x04: #SLLV
			s = regs[rs] & 0b1111
			#regs[rd] = regs[rt] << s
                        val = regs[rt]
                        for i in range(0,s):
                            val <<= 1
                        regs[rd] = val & 0xFFFFFFFF
                        PC = PC + 4
		else if funct == 0x06: #SRLV
			s = regs[rs] & 0b1111 
                        val = regs[rt]
			#regs[rd] = regs[rt] & (2**31)) + (regs[rt] >> s)
                        for i in range(0,s):
                            val >>= 1
                        regs[rd] = val & 0xFFFFFFFF
                        PC = PC + 4
		else if funct == 0x07: #SRAV
			s = regs[rs] & 0b1111
                        val = regs[rt]
			#regs[rd] = (regs[rt] & (2**31)) + (regs[rt] >> s)
                        for i in range(0,s):
                            val >>= 1
                            val |= 1
                        regs[rd] = val & 0xFFFFFFFF
                        PC = PC + 4
		else if funct == 0x08: #JR
			temp = regs[rs]
                        if (temp & 0x00000003 == 0): #0..11
                            PC = temp
                        else:
                            Cause_Reg = 0
                            PC = KERNEL_MODE
			PC = temp
		else if funct == 0x09: #JALR
			temp = regs[rs]
			regs[rd] = PC + 8
                        if (temp & 0x00000001 != 0) or (temp & 0x00000002 != 0): #0..01 or 0..10 
                            Cause_Reg = 0
                            PC = KERNEL_MODE
                        else:
                            PC = temp
		else if funct == 0x10: #MFHI
			regs[rd] = HI
			PC = PC + 4
		else if funct == 0x0C: #SYSCALL
                        Cause_Reg = 8
			PC = KERNEL_MODE
		else if funct == 0x0d: # BREAK
                        Cause_Reg = 9
			PC = KERNEL_MODE
		else if funct == 0x0E: #XORI
			regs[rt] = regs[rs] ^ zero_extend(inmediate)
			PC = PC + 4
		else if funct == 0x12: #MFLO
			regs[rd] = LO
			PC = PC + 4
		else if funct ==0x18: #MUL
			temp = bytestoint32(regs[rs]) * bytestoint32(regs[rt])
			LO = uint32tobytes(((temp & 0x00000000FFFFFFFF) << 32) >> 32)
			HI = uint32tobytes(((temp & 0xFFFFFFFF00000000) >> 32))
                        PC = PC + 4
		else if funct == 0x19: #MULU
			temp = bytestouint32(regs[rs]) * bytestouint32(regs[rt])
			LO = uint32tobytes(((temp & 0x00000000FFFFFFFF) << 32) >> 32)
			HI = uint32tobytes(((temp & 0xFFFFFFFF00000000) >> 32))
			PC = PC + 4
                else if funct == 0x1A: #DIV 
                        try:
				tmp1 = bytestoint32(regs[rs]) / bytestoint32(regs[rt])
				tmp2 = bytestoint32(regs[rs]) % bytestoint32(regs[rt])
				LO = int32tobytes(tmp1)
				HI = int32tobytes(tmp2)
                                PC = PC + 4
                        except:
                                EPC = PC
                                DIV0 = 1
                                Cause_Reg = 12
                                PC = KERNEL_MODE + 0x80
                else if funct == 0x1B: #DIVU
                        try:
                                tmp1 = bytestouint32(regs[rs]) / bytestouint32(regs[rt])
                                tmp2 = bytestouint32(regs[rs]) % bytestouint32(regs[rt])
				LO = uint32tobytes(tmp1)
				HI = uint32tobytes(tmp2)
                                PC = PC + 4
                        except:
                                EPC = PC
                                DIV0 = 1
                                Cause_Reg = 12
                                PC = KERNEL_MODE + 0x80
		else if funct == 0x20: #ADD
			temp = bytestoint32(regs[rs]) + bytestoint32(regs[rt])
			s1 = (bytestouint32(regs[rs]) & mask) >> 31
			s2 = (bytestouint32(regs[rt]) & mask) >> 31
			t = (temp & mask) >> 31
			OF = (t == int(not(s1 ^ s2))) 
			if not OF:
				regs[rd] = int32tobytes(temp)
				PC = PC + 4
			else:
			  	Cause_Reg = 0b1100
                                PC = KERNEL_MODE + 0x80	
		else if funct == 0x21: #ADDU 
			temp = bytestoint32(regs[rs]) + bytestoint32(regs[rt]) % 0x7fffffff
			regs[rd] = int32tobytes(temp)
                        PC = PC + 4
		else if funct == 0x22: #SUB
			temp = bytestoint32(regs[rs]) - bytestoint32(regs[rt])
                        s1 = (bytestouint32(regs[rs]) & mask) >> 31
                        s2 = (bytestouint32(regs[rt]) & mask) >> 31
                        t = (temp & mask) >> 31
			OF = (s1 == (t ^ s2))
			if OF:
                                Cause_Reg = 0b1100
                                PC = KERNEL_MODE + 0x80
                        else:
				regs[rd] = int32tobytes(tmp)
                                PC = PC + 4	
		else if funct == 0x23: #SUBU
			temp = bytestoint32(regs[rs]) - bytestoint32(regs[rt]) % 0x7fffffff
			regs[rd] = int32tobytes(temp)
                        PC = PC + 4	
		else if funct == 0x24: #AND
			temp = bytestouint32(regs[rs]) & bytestouint32(regs[rt])
			regs[rd] = uint32tobytes(temp)
			PC = PC + 4
		else if funct == 0x25: #OR
			temp = bytestouint32(regs[rs]) | bytestouint32(regs[rt])
			regs[rd] = uint32tobytes(temp)
                        PC = PC + 4
		else if funct == 0x26: #XOR
			temp = bytestouint32(regs[rs]) ^ bytestouint32(regs[rt])
			regs[rd] = uint32tobytes(temp)
			PC = PC + 4
		else if funct == 0x27: #NOR
			temp = (bytestouint32(regs[rs]) | bytestouint32(regs[rt])) ^ REGBITS
			regs[rd] = uint32tobytes(temp)
			PC = PC + 4
		else if funct == 0x2A: #SLT		
			if bytestoint32(regs[rs]) < bytestoint32(regs[rt]):
				regs[rd] = bytestouint32(1)
			else
				regs[rd] = bytestouint32(0)
			PC = PC + 4
		else if funct == 0x2B: #SLTU		
			if bytestouint32(regs[rs]) < bytestouint32(regs[rt]):
                                regs[rd] = bytestouint32(1)
                        else
                                regs[rd] = bytestouint32(0)
			PC = PC + 4
		else:
			EPC = PC
                        Cause_Reg = 10
                        PC = KERNEL_MODE + 0x80

		print "R opcode,rt,rs,rd,shamt,funct",opcode,rs,rt,rd,shamt,funct

	else if opcode > 3: # I
		#OOOOOOssssstttttiiiiiiiiiiiiiiii
		rs = (instruction[0] & 0b00000011) << 8 + ((instruction[1] & 0b11100000) >> 5)
	        rt = instruction[1] & 0b00011111
        	inmediate = instruction[2] + instruction[3]

		if opcode == 0x01: # BLTZ/BLTAL
			if rt == 0x00: # BLTZ
				tgt_offset = bytestoint32(inmmediate) << 2
				if bytestoint32(regs[rs]) < 0:
					PC = PC + tgt_offset
				else:
					PC = PC + 4
			else if rt == 0x01: #BLTZAL
				tgt_offset = bytestoint32(inmediate) << 2
				regs[31] = PC + 8
				if bytestoint32(regs[rs]) < 0:
					PC = PC + tgt_offset
				else		
					PC = PC + 4
		if opcode == 0x08: #ADDI
			temp = bytestoint32(regs[rs]) + bytestoint32(inmediate) 
			s1 = (bytestouint32(regs[rs]) & mask) >> 31
			s2 = (bytestouint32(inmediate) & mask) >> 31
			t = (temp & mask) >> 31
			#print s1,s2,t
			OF = (t == int(not(s1 ^ s2)))
			if OF == 1:
				EPC = PC
				Cause_Reg = 0b1100
				PC = KERNEL_MODE + 0x80
			else:
				regs[rt] = int32tobytes(temp)
				PC = PC + 4
		else if opcode == 0x09: #ADDIU
			temp = (bytestoint32(regs[rs]) + bytestoint32(inmediate)) % 0x7fffffff 
			regs[rt] = int32tobytes(temp) 
			PC = PC + 4
		else if opcode == 0x0C: #ANDI 
			temp = bytestouint32(regs[rs]) & bytestouint32(inmediate)
			regs[rt] = temp
			PC = PC + 4
		else if opcode == 0x04: #BEQ 
			tgt_offset = bytestoint32(inmediate) << 2
			if regs[rs] = regs[rt] 
				PC = PC + tgt_offset
			else:
				PC = PC + 4
		else if opcode == 0x05: #BNEQ
			tgt_offset = bytestoint32(inmediate) << 2
			if regs[rs] != regs[rt] 
                                PC = PC + tgt_offset
                        else:
                                PC = PC + 4
		else if opcode == 0x06: #BLEZ 
			tgt_offset = bytestoint32(inmediate) << 2
			if bytestoint32(regs[rs]) <= 0:
				PC = PC + tgt_offset
			else:
				PC = PC + 4
		else if opcode == 0x07: #BGTZ
			tgt_offset = bytestoint32(inmediate) << 2
			if bytestoint32(regs[rs]) > 0:
				PC = PC + tgt_offset
			else:
				PC = PC + 4

		else if opcode == 0x10: # MFEPC/MFCO
			if rt = 0b01110:
				regs[rd] = EPC
			if rt = 0b01101:
				regs[rd] = Cause_Reg
			PC = PC + 4

		else if opcode == 0x0A: #SLTI
			if bytestoint32(regs[rs]) < bytestoint32(inmediate):
				regs[rt] = bytestouint32(1)
			else:
				regs[rt] = bytestouint32(0)
			PC = PC + 4	

		else if opcode == 0x0C: #SLTIU
                	if bytestouint32(regs[rs]) < bytestoint32(inmediate):
                        	regs[rt] = bytestouint32(1)
                        else:
                                regs[rt] = bytestouint32(0)
                        PC = PC + 4

		else if opcode == 0x0D: #ORI
			regs[rt] = regs[rs] | zero_extend(inmediate) 
			PC = PC + 4

		else if opcode == 0x20:	# LB 
			base = bytestouint32(regs[rs]) + bytestoint32(inmediate)
			try:
				temp = memory[base:base+4]
				s = (ord(temp[0]) & 0x80) >> 7
				temp = zero_extend(temp[0])
				temp[3] = chr(ord(temp[3]) | s 
				regs[rt] = temp
				PC = PC + 4
			except:
				EPC = PC
                                Cause_Reg = 0b100 # 4
                                PC = KERNEL_MODE + 0x80
		else if opcode == 0b100100: #LBU
			base = bytestouint32(regs[rs]) + bytestoint32(inmediate)
                        try:
                                temp = zero_extend(memory[base:base+4])
				regs[rt] = zero_extend(temp[0])
                                PC = PC + 4
                        except:
                                EPC = PC
                                Cause_Reg = 0b100 # 4
                                PC = KERNEL_MODE + 0x80
		else if opcode == 0x23: # LW 
			base = bytestouint32(regs[rs]) + bytestoint32(inmediate)
			try:
				if (base & 0b10 == 0b10) or (base & 0b01 == 0b01):
					EPC = PC
                                	Cause_Reg = 0b100 # 4
					PC = KERNEL_MODE + 0x80
				else:
					regs[rt] = memory[base:base+4]
					PC = PC + 4
			except:
				EPC = PC
				Cause_Reg = 0b100 # 4
				PC = KERNEL_MODE + 0x80

		else if opcode == 0x28: # SB WIP
			memory[rs+(inmediate & 0x0000FFFF)] = regs[rt] & 0x000000FF
			PC = PC + 4 
		else if opcode == 0x2b: # SW WIP
			try:
				memory[rs+(inmediate)+0] = (regs[rt] & 0b00000000000000000000000011111111)
				memory[rs+(inmediate)+1] = (regs[rt] & 0b00000000000000001111111100000000) >> 8
				memory[rs+(inmediate)+2] = (regs[rt] & 0b00000000111111110000000000000000) >> 16
				memory[rs+(inmediate)+3] = (regs[rt] & 0b11111111000000000000000000000000) >> 24
				PC = PC + 4
			except:	
				EPC = PC
				Cause_Reg = 0b101 # 5
				PC = KERNEL_MODE + 0x80
		else:
                	EPC = PC
	                Cause_Reg = 0b1010
        	        PC = 0x80000180

		print "I opcode,rt,rs,inmediate",opcode,rs,rt,inmediate		

	else if opcode == 0x02 and opcode == 0x03: # J
		#OOOOOOaaaaaaaaaaaaaaaaaaaaaaaaaa	
		address = (instruction[0] & 0b11000000 >> 6) * 0xff + instruction[1]
		if opcode == 0x2: #J
			PC = (PC & 0xf0000000) | (address << 2)
		if opcode == 0x3: #JAL
			regs[31] = PC + 8
			PC = (PC & 0xf0000000) | (address << 2)
		print "J opcode,address",opcode,address,

	else:
		EPC = PC
		Cause_Reg = 0b1010
                PC = KERNEL_MODE + 0x80

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
        RUN = True
	ramfile = sys.argv[1]
	load_ram(ramfile)
        PC = PROCESOR_RESET # Set PC <- PROCESOR RESET VECTOR
	while RUN:
		instruction = memory[PC:PC+4]
		decode_and_execute(instruction)
	save_ram(ramfile)

execute()
