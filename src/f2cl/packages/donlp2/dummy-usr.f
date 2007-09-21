      SUBROUTINE SETUP0
c      INCLUDE 'O8COMM.INC'
      RETURN
      END

C     OBJECTIVE FUNCTION
      SUBROUTINE EF(X,FX)
c      INCLUDE 'O8FUCO.INC'
      RETURN
      END
C     GRADIENT OF OBJECTIVE FUNCTION
      SUBROUTINE EGRADF(X,GRADF)
c      INCLUDE 'O8FUCO.INC'
      RETURN
      END

C  COMPUTE THE I-TH EQUALITY CONSTAINT, VALUE IS HXI
      SUBROUTINE EH(I,X,HXI)
c      INCLUDE 'O8FUCO.INC'
      RETURN
      END
C  COMPUTE THE GRADIENT OF THE I-TH EQUALITY CONSTRAINT
      SUBROUTINE EGRADH(I,X,GRADHI)
c      INCLUDE 'O8FUCO.INC'
      RETURN
      END

C COMPUTE THE I-TH INEQUALITY CONSTAINT, BOUNDS INCLUDED

      SUBROUTINE EG(I,X,GXI)
c      INCLUDE 'O8FUCO.INC'
      RETURN
      END
C COMPUTE THE GRADIENT OF THE I-TH INEQUALITY CONSTRAINT
C NOT NECESSARY FOR BOUNDS, BUT CONSTANT GRADIENTS MUST BE SET
C HERE E.G. USING DCOPY FROM A DATA-FIELD
      SUBROUTINE EGRADG(I,X,GRADGI)
c      INCLUDE 'O8FUCO.INC'
      RETURN
      END