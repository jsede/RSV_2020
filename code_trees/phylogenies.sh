#!/bin/bash

#add RSV-A study sequences to reference alignment
mafft --thread 48 --add GenBank.RSVA.plusStudy.fasta RSVA.ref.fa > GenBank.RSVA.combined.aligned.fasta

#estimate a quick test tree to examine for any outliers
fasttree -nt -gtr GenBank.RSVA.combined.aligned.fasta > GenBank.RSVA.combined.aligned.tree

#repeat for RSV-B alignments
mafft --thread 48 --add GenBank.RSVB.plusStudy.fasta RSVB.ref.fa > GenBank.RSVB.combined.aligned.fasta
fasttree -nt -gtr GenBank.RSVB.combined.aligned.fasta > GenBank.RSVB.combined.aligned.tree

#remove any unwanted sequences (excessively divergent, wrong dates or low quality)

#estimate final trees
raxmlHPC-PTHREADS-AVX2 -T 48 -f a -x 29283 -p 28372 -N 1000 -m GTRGAMMA -s GenBank.RSVA.combined.aligned.fasta -n RSVA.alignment.GTRGAMMA.B1000 &
raxmlHPC-PTHREADS-AVX2 -T 48 -f a -x 19191 -p 18272 -N 1000 -m GTRGAMMA -s GenBank.RSVB.combined.aligned.fasta -n RSVB.alignment.GTRGAMMA.B1000 &
