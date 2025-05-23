*###############################################*
*       ___        __         ___               *
*      / __\___   / /  ___   / __\___  _ __     *
*     / /  / _ \ / /  / _ \ / /  / _ \| '__|    *
*    / /__| (_) / /__| (_) / /__| (_) | |       *
*    \____/\___/\____/\___/\____/\___/|_|       *
*                                               *
*###############################################*
               T2P_CoLocCor Readme
		  S.S. Papadopulos & Associates
                   5/23/2025       
*-----------------------------------------------*

**T2P_CoLocCor** is a small utility program designed to detect and correct
co-located wells in input files prepared for use with **Texture2Par**.

Texture2Par will fail when wells or logs are  exactly co-located in space,
as kriging-based interpolation routines require spatially distinct data points.

T2P_CoLocCor:
- Reads in a Texture2Par-format dataset file
- Checks for wells (logs) that share the exact same X,Y coordinates
- Slightly adjusts the position of duplicate wells to make them unique
- Writes a corrected output file
- Logs all modifications to a file called `CoLoCor_Moved.log`

This is a preprocessor to ensure successful execution of Texture2Par.

---

**Usage:**

Run from the command line with:

T2P_CoLocCor.exe [input_file] [output_file] [nclasses] [nhsus]

Where:
- `input_file`  = original Texture2Par dataset (e.g., `mylog.csv`)
- `output_file` = name of the corrected file to create
- `nclasses`    = number of texture classes (e.g., 1)
- `nhsus`       = number of columns of HSUs (the number of layers in the model)

---

**Output:**
- `[output_file]`: the new dataset file, with co-located wells slightly separated
- `CoLoCor_Moved.log`: a human-readable log of each modified well, including:
  - Well name
  - Original and new coordinates
  - Distance moved

---

**Notes:**
- Wells are moved by small amounts (+0.01 units per duplicate) to preserve local structure.
- If your original file has no co-located wells, it will simply be copied to the output unchanged.

---

**Source Code & Updates:**
The latest version is available at:
https://github.com/scantle/T2P_CoLoCor