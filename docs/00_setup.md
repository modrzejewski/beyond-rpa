# Setup

The project uses the Meson build system. The compiler flags and dependencies are configured using profile files located in `.meson/profiles/`.

1. Clone the repository:
   ```bash
   git clone https://github.com/modrzejewski/beyond-rpa.git
   cd beyond-rpa
   ```

2. Setup the build directory with a chosen compiler profile (e.g., `ifx-I64.ini`):
   ```bash
   meson setup build --native-file .meson/profiles/ifx-I64.ini
   ```

3. Compile the project:
   ```bash
   cd build
   meson compile -j 4
   ```
   *Note: Using a parallel build (e.g., `-j 4`) is highly recommended. It significantly speeds up the stage where we build a large number of individually optimized integral subroutines.*
