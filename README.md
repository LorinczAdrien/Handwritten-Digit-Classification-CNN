# Handwritten Digit Classification CNN - x86 Assembly

- Handwritten Digit Classification using a Convolutional Neural Network trained on the MNIST database - Written in x86 Assembly.

## Important

- Comments written in Hungarian
- This implementation does **NOT** implement backpropagation (or any other form of learning). The weights and biases are read from the binary file 'conv_model.bin' and the structure (layers) of the CNN are read and processed from the file 'conv_model.txt'.

## Implementation details

- This implementation contains a **GUI**, where the user can draw a number on a blank canvas, than reset/accept (red/green buttons) it. If the user the user accepts the drawing, the image is fed into the **CNN**. After processing, the output containing all the possible outcomes associated each with their probabilities (guesses) and finally the digit having the highest probability is shown in the terminal.
- Trained on the [MNIST database](https://en.wikipedia.org/wiki/MNIST_database).

- Implemented operations
  - **Preprocessing:** Downscaling input image to a 28x28 size, scaling the pixel values between [-1,1]. Reading the structure (layers) of the CNN from a file. Reading the weights and biases from a file.
  - **ReLU layer**
  - **Padding operation**
  - **Fully connected layers**: The project was initially a **multilayer perceptron** (**MLP**)
  - **Max pooling layer:** Kernel size: 2x2, Stride: 2
  - **Convolution layer:** Kernel size: 3x3, Padding: 1, Stride: 1
  - **Softmax layer**
  
- Other
  - General speed optimizations with **SSE** and **AVX** instructions.

## Build

- nasm -f win32 projekt.asm
- nlink projekt.obj -lio -lmio -lgfx -lutil -o projekt.exe
