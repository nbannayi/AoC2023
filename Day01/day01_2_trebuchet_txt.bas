10 rem advent of code 2023, day 01, part 2 - trebuchet?!
20 rem commodore 64 basic, takes about 30 mins
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
85 print "part 2 answer:";t
95 end
100 rem extract digits from line and concatenate (ascii 48-57), also parse words
110 l = -1 : r = -1
120 for j = 1 to len(v1$(i))
130    c3$ = mid$(v1$(i),j,3)
140    c4$ = mid$(v1$(i),j,4)
150    c5$ = mid$(v1$(i),j,5) 
160    if c3$ = "one" then c1$ = "1" : j = j + 1 : goto 260
170    if c3$ = "two" then c1$ = "2" : j = j + 1 : goto 260
180    if c3$ = "six" then c1$ = "6" : j = j + 2 : goto 260
190    if c4$ = "five" then c1$ = "5" : j = j + 2 : goto 260
200    if c4$ = "four" then c1$ = "4" : j = j + 3 : goto 260
210    if c4$ = "nine" then c1$ = "9" : j = j + 2 : goto 260
220    if c5$ = "eight" then c1$ = "8" : j = j + 3 : goto 260
230    if c5$ = "seven" then c1$ = "7" : j = j + 3 : goto 260
240    if c5$ = "three" then c1$ = "3" : j = j + 3 : goto 260
250    c1$ = mid$(v1$(i),j,1)
260    c = asc(c1$)
270    if l > 0 and c >= 48 and c <= 57 then r = val(c1$) 
280    if l < 0 and c >= 48 and c <= 57 then l = val(c1$)
290 next j
300 if r > 0 then v2%(i) = 10*l+r
310 if r < 0 then v2%(i) = 10*l+l
320 return
240 rem
250 rem puzzle input
1000 rem data redacted but put other data statements here
1010 rem data here
10990 rem data here