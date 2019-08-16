Objective
Read Chapter 21 of the HCS12 Microcontroller data sheet MC9S12C Family. And program 40 bytes of data to the flash memory starting at $4800.

Instruction
Finish reading Chapter 18 of the HCS12 Microcontroller data sheet.

Study the sample HW11 program to store a short program in flash memory: 
sample HW11 program for HCS12C128 board 

Write your HW11 program to write 40 bytes to flash memory starting at $4800.

First 4 bytes of the 40 byte data must be your student ID number (last 4 digits) in ASCII code. For example, my ID will be $30, $31, $32, $33 (that is '0123'). The remaining data must be an exact copy of the data block given in the sample HW11 program.

The sample HW11 program shows flash memory writing subroutine. You must write flash memory sector erase subroutine.

Flash memory must be erased before the data can be written. Because you will be writing only 40 bytes, erase only one sector (1024 bytes) starting at $4800.

Design the program to start at $3100 and data to start at $3000 as usual.