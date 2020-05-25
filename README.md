# Bootloader Snake
A basic version of Snake written in x86 Assembly which fits into a 512 bytes bootloader

[![bootloader_snake_demo.gif](https://s7.gifyu.com/images/bootloader_snake_demo.gif)](https://gifyu.com/image/nC6S)
## Compile
```Shell
nasm snake.asm -fbin -o snake.bin
```

## Run
This game has been tested on Bochs and VirtualBox
### Create an img file for VirtualBox
```Shell
dd if=/dev/zero of=snake.img bs=1024 count=1440
dd if=snake.bin of=snake.img conv=notrunc
```

## Known Issues / Area for Improvement
>* Game speed remains constant, notta very exciting
>* "Candy" could spawn under the snake