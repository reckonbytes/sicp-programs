I found it hard to keep track of the saves & restores, particularly for the continue register. So, I have modified my machine's instructions to:
```
label-save <label-name> <reg1> ... <regn>)
&
(label-restore)
```
where \<reg1\> ... \<regn\> are optional arguments listing the saved register names.

Also, a label is followed by a 'restored-regs' expression:
```
<label-name>
(restored-regs <reg1> ... <regn>)
```
'restored-regs' is not really an instruction. It is scanned out by the assembler and not included in the machine's instruction sequence. But, the assembler signals an error if restored-regs' arguments do not match the registers saved with the label-name. The 'restored-regs' "instruction" exists only for the programmer to understand and check controller code.
