var a = 40;
var b = 43;
var c = true;
var cont = 0;
while(c) {
if( a < b){
  ++a;
  ++cont;
} else {
  if(a == b) {
  c = false;
  }
}
}
[cont++];
