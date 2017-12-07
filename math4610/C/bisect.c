#include <stdio.h>

#include <math.h>

main()

{
        float a, left, right, middle;
        float left_residual, right_residual, middle_residual,tol=1e-6;
        int j;
        a=2.0;
        left=1.0;
        left_residual=left*left-a;
        right=3.0;
        right_residual=right*right-a;
        middle=(right + left)/2.0;
        middle_residual=middle*middle-a;
//      for (j=0; j<10 ; j++)  // calculate sum a_j x^j

        while (fabs(middle_residual)>tol)

        {
                printf("[%f,%f], %f \n",left,right, middle);
                if (middle_residual*left_residual>0)
                        {left=middle; left_residual=middle_residual;}
                else
                        {right=middle; right_residual=middle_residual;}
                middle=(right + left)/2.0;
                middle_residual=middle*middle-a;
        }
}
