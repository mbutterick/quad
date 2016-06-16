#lang brag

doc : multipage [/PAGE-BREAK multipage]*

multipage : multicolumn [/COLUMN-BREAK multicolumn]*

multicolumn : multiblock [/BLOCK-BREAK multiblock]*

multiblock : multiline [/LINE-BREAK multiline]*

multiline : QUAD*