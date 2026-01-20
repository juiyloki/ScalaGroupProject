# ScalaGroupProject

Final group project for the **Scala course at MIMUW**.

**Authors:**  
- Agata Kopeć  
- Anna Szkólska  
- Nina Wiśniewska  
- Łukasz Górny  

---

## About the Project

This project is a fully functional **Sudoku game** implemented in **Scala** using **JavaFX**.  
It provides a smooth and user-friendly gameplay experience with multiple quality-of-life features.

---

## Running Instructions

To run the program on **Ubuntu 24.04**, type the following commands:

```bash
sbt compile
sbt run
```

---

### Prerequisites

Make sure to have **sbt** installed. You can find the official installation guide here:
[https://www.scala-sbt.org/download.html](https://www.scala-sbt.org/download.html)

---

## Features

- Gameplay on a 9×9 board using buttons
- Menu screen with difficulty selection
- Rules tab explaining how to play
- Timer showing how long the game takes
- Lives system, that limits user mistakes and ends the game if it is exceeded
- **X button** for removing previously written numbers
- **Hint button** that fills in a valid number
- Undo button to revert previous moves
- Highlighting of:
  - all identical digits when one is selected
  - row and column of the selected cell
  - completed row / column / 3×3 box (green highlight)
- Red color for invalid moves
- Winning screen with completion time

---

## Controls

- **Mouse**: Click cells to select, click on-screen keypad to enter numbers.
- **Keyboard**:
  - `Arrow Keys`: Move selection.
  - `1-9`: Enter number.
  - `0`, `Backspace`, `Delete`: Clear cell.
  - `Ctrl + Z`: Undo last move.
  - `H`: Use a hint.
  - `M`: Return to menu.

---

## Screenshots

### Opening View
<img width="712" height="790" alt="opening view" src="https://github.com/user-attachments/assets/ff05f633-ac12-4b20-a062-488f890545e1" />

---

### Rules Tab
<img width="712" height="790" alt="rules tab" src="https://github.com/user-attachments/assets/5b71ed05-ca11-4c8d-8033-df3933445a08" />

---

### Gameplay
<img width="712" height="790" alt="gameplay" src="https://github.com/user-attachments/assets/00d7be5e-280b-4311-8386-4f8a86df3154" />

---

## Highlighting Examples

### Highlighting All Equal Numbers
<img width="712" height="790" src="https://github.com/user-attachments/assets/f29db201-6bdd-4cf4-8a54-1a84deddcd95" />

---

### Highlighting Row and Column
<img width="712" height="790" alt="Screenshot 2026-01-20 at 09 15 20" src="https://github.com/user-attachments/assets/46e414ac-f6f2-403e-8d1d-45b3d68114a5" />

---

### Completed 3×3 Box and Row
<img src="https://github.com/user-attachments/assets/e2f53520-ee14-484a-8c15-4f0c82f649ec" width="600" />

---

### 6x6 mode
<img width="712" height="790" alt="Screenshot 2026-01-20 at 09 15 55" src="https://github.com/user-attachments/assets/e2ef4305-b104-440a-85b1-2dff255d90f8" />

---

### Pink mode
<img width="712" height="790" alt="Screenshot 2026-01-20 at 09 16 22" src="https://github.com/user-attachments/assets/7e77e998-f366-476c-b7c3-2a8dfe4f2e94" />

---



## Winning Screen
<img src="https://github.com/user-attachments/assets/bbb28c4b-32ac-4a08-9e54-3274072f5ff3" width="600" />
