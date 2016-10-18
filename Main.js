var a = 40;
var b = 43;
var c = 0;
for (a; a <= b; a++)
{
	c++;
	if(c==1){
		break;
		c = 42;
	}else{
		c--;
	}
}
c;
