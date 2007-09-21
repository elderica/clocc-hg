C****************************************************************
      SUBROUTINE O8RINV(NDIM,N,A,NDUALM,NDUAL,X)
C******* COMPUTES THE INVERSE OF THE UPPER TRIANGULAR MATRIX PART
C******* OF A AND STORES IT IN THE UPPER TRIANGLE OF THE
C******* RIGHT LOWER MINOR OF X
C******* DECLARATION DIMENSION OF A IS NDIM, OF  X IS NDUALM
C******* ACTUAL DIMENSION OF A IS N AND OF X NDUAL
      IMPLICIT NONE
      INCLUDE 'O8CONS.INC'
      INTEGER NDIM,NDUALM,N,NDUAL,L,J,K,INCR
      DOUBLE PRECISION A(NDIM,*),X(NDUALM,NDUALM),SU
      SAVE
      INCR=NDUAL-N
C***  INCR=NR
      DO J=N,1,-1
C*** WE ASSUME A BEING SUFFICIENTLY REGULAR HERE.
C*** GIVEN IN THIS APPLICATION
C*** SEE TOP OF O8QPDU
        X(J+INCR,J+INCR)=ONE/A(J,J)
        DO K=J-1,1,-1
          SU=ZERO
          DO L=K+1,J
            SU=SU+A(K,L)*X(L+INCR,J+INCR)
          ENDDO
          X(K+INCR,J+INCR)=-SU/A(K,K)
        ENDDO
      ENDDO
      RETURN
      END
