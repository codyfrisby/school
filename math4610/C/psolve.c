#include <stdio.h>
#include <math.h>
const maxdeg=5;

float p(float a[],int deg,float x);

main()
	
{

	float c, left, right, middle;
	float lres, rres, mres, tol=1e-6;
	float a[maxdeg+1];
	int j, n;

	printf("We will use this method to solve p(x)=c. Enter c:");
	scanf("%f",&c);
	printf("Enter the degree of p(x), n:");
	scanf("%d",&n);
	for (j=n; j>=0; j--) 
	{
		printf("Enter the coefficient of p(x) of degree %d, a[%d]:", j, j);
		scanf("%f", &a[j]);
	}
	printf("Enter the left endpoint of a bracketing interval, l:");
	scanf("%f",&left);
	printf("Enter the right endpoint of a bracketing interval, r:");
	scanf("%f",&right);
	lres=p(a,n,left)-c;
	rres=p(a,n,left)-c;
	middle=(right + left)/2.0;
	mres=p(a,n,middle)-c;
	while (fabs(mres)>tol)
	{
	printf("[%f,%f], %f \n", left,right,middle);

	if (mres*lres>0)
		{left=middle; lres=mres;} 
	else
	{right=middle; rres=mres;}

	middle=(right+left)/2.0;
	mres=middle*middle-c;
	}	
}

float p( float a[], int deg, float x )
{
float sum=0.0;int j;

for (j=0;j<=deg;j++){
sum+=a[j]*pow(x,j);
}
return sum;
}
