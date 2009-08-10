#

set datafile separator ",";
set data style lines;

plot 't.csv' using ($1/100) ti column(1),\
  '' u 2 ti column(2),\
  '' u 3 ti column(3),\
  '' u 4 ti column(4),\
  '' u 5 ti column(5),\
  '' u 6 ti column(6),\
  '' u 7 ti column(7),\
  '' u 8 ti column(8),\
  '' u ($9/1000) ti column(9),\
  '' u ($10/1000) ti column(10),\
  '' u 11 ti column(11),\
  '' u 12 ti column(12),\
  '' u ($13/1000) ti column(13),\
  '' u 14 ti column(14),\
  '' u 15 ti column(15),\
  '' u 16 ti column(16),\
  '' u 17 ti column(17),\
  '' u 18 ti column(18),\
  '' u ($19*10) ti column(19),\
  '' u 20 ti column(20),\
  '' u 21 ti column(21);

