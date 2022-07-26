      CHARACTER  datafile*100,argv*100,IN_STR*10
      REAL inweights(10,64)      ! input layer: 64 neurons, 10 weights/neuron
      REAL aiweights(50, 64, 64) ! hidden layers: 64 neurons, 64 weights/neuron, 50 layers
      REAL outweights(64,1)      ! output layer: 1 output (Qr = (kg H20(liq.))/(kg Air)), 64 weights

        do i = 1, iargc()
          if (i==1) call getarg(i, argv)
        end do
        READ(argv, *) datafile
        print*, "infile = ",datafile

        call readData(datafile, inweights, aiweights, outweights, 10,50,64)
        print *, "JUST A TEST"
        print *, "FIRST INPUT BLOCK"
        do i = 1, 10
          print *, inweights(i,:)
          print *,""
        end do
      end

      SUBROUTINE readData(filename, inw, aiw, outw, in_num, block_num, ai_num)
      INTEGER in_num, block_num, ai_num
      REAL inw(in_num,ai_num), aiw(block_num, ai_num, ai_num), outw(ai_num,1)      
      CHARACTER  filename*100
      CHARACTER  LINE*1200
      CHARACTER  tLINE*1200
      CHARACTER  ch, lastEntry*100
      INTEGER    b, l, c

      open (unit=1, file=filename, form='formatted')      
      !parse the encoding block
      do l = 1,in_num
        read(1, "(A)", iostat=io) LINE
        tLINE= LINE(3:)
        READ(tLINE, *) inw(l,1) 
        tLINE= tLINE(16:)
        do c = 2,ai_num-1
           READ(tLINE, *) inw(l,c) 
           tLINE= tLINE(17:)
        end do
        READ(tLINE, *) lastEntry
        lastEntry= lastEntry(1:15)
        if (lastEntry(15:15) == ']') lastEntry=lastEntry(1:14)        
        READ(lastEntry, *) inw(l,ai_num)
      end do        

      !skip 3 lines
      read(1, "(A)", iostat=io) LINE
      read(1, "(A)", iostat=io) LINE
      read(1, "(A)", iostat=io) LINE

      !parse the compute block
      do b = 1,block_num
        do l = 1,ai_num
           read(1, "(A)", iostat=io) LINE
           tLINE= LINE(3:)
           READ(tLINE, *) aiw(b,l,1)
           tLINE= tLINE(16:)
           do c = 2,ai_num-1
               READ(tLINE, *) aiw(b,l,c)
               tLINE= tLINE(17:)
           end do
           READ(tLINE, *) lastEntry
           lastEntry= lastEntry(1:15)
           if (lastEntry(15:15) == ']') lastEntry=lastEntry(1:14)
           READ(lastEntry, *) aiw(b,l,ai_num)
        end do
        !skip 3 lines
        read(1, "(A)", iostat=io) LINE
        read(1, "(A)", iostat=io) LINE
        read(1, "(A)", iostat=io) LINE
      end do

      !parse the output block
      do l = 1,ai_num
         read(1, "(A)", iostat=io) LINE
         tLINE= LINE(3:)
         if (tLINE(15:15) == ']') tLINE=tLINE(1:14)
         READ(tLINE, *) outw(l,1)
      end do

      CLOSE (1, STATUS='KEEP')
      return              

      end
