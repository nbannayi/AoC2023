10 rem advent of code 2023, day 01, part 1 - trebuchet?!
20 rem commodore 64 basic (note runs in around 16 mins 30 secs)
30 rem
40 rem main loop, read all data lines in turn
50 dim v1$(1000)
55 dim v2%(1000)
60 for i = 0 to 999
70    read v1$(i)
75    gosub 100
76    t = t + v2%(i)
78    print i+1;v1$(i);v2%(i);t
80 next i
85 print "part 1 answer:";t
90 end
100 rem extract digits from line and concatenate (ascii 48-57)
116 l = -1 : r = -1
120 for j = 1 to len(v1$(i))
122    c$ = mid$(v1$(i),j,1)
125    c = asc(c$)
126    if l > 0 and c >= 48 and c <= 57 then r = val(c$) 
130    if l < 0 and c >= 48 and c <= 57 then l = val(c$)
150 next j
160 if r > 0 then v2%(i) = 10*l+r
170 if r < 0 then v2%(i) = 10*l+l
180 return
190 rem
200 rem puzzle input
1000 data "gtlbhbjgkrb5sixfivefivetwosix"
1010 rem data redacted but put other data statements here
10990 data "eightrtsjszc2"
