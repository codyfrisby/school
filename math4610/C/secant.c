#include <stdio.h>
#include <math.h>

float f(float x)
{
    return(x*x-2); // f(x)= x^2-2
}
float main()
{
    float a,b,c,d,e;
    int count=1,n;
    printf("Enter the values of a and b:\n"); //(a,b) must contain the solution.
    scanf("%f%f",&a,&b);
    printf("Enter the values of allowed error and maximun number of iterations:\n");
    scanf("%f %d",&e,&n);
    do
    {
        if(f(a)==f(b))
       {
           printf("\nSolution cannot be found as the values of a and b are same.\n");
      return 0; 
       }
       c=(a*f(b)-b*f(a))/(f(b)-f(a));
       a=b;
       b=c;
       printf("Iteration No-%d    x=%f\n",count,c);
       count++;
       if(count==n)
       {
       break;
       }
    } while(fabs(f(c))>e);
    printf("\n The required solution is %f\n",c);
  return 0;
}
