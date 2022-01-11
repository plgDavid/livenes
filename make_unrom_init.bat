del live_UNROM_init.nes
asm6 file.asm -dUNROM -dCHRRAM -dINIT_2A03
IF EXIST file.bin. (
move file.bin live_UNROM_init.nes
Nintendulator live_UNROM_init.nes
)
