//quicksort
function getMenores(list, pivo) {
	if(list.equals([])) {
		return [];
	} else {
		if((list.head) < pivo) {
			return (([head(list)]).concat(getMenores(tail(list), pivo)));
		} else {
			return ((tail(list)).getMenores(pivo));
		}
	}
}

function getMaiores(list, pivo) {
	if(list.equals([])) {
		return [];
	} else {
		if((list.head) >= pivo) {
			return ([list.head]).concat((list.tail).getMaiores(pivo));
		} else {
			return ((list.tail).getMaiores(pivo));
		}
	}
}

function quicksort(list) {
	if (list.equals([])) {
		return [];
	} else {
	      var t = (((list.tail).getMenores(list.head)).quicksort).concat(list.head)
        return t.concat(((list.tail).getMaiores(list.head)).quicksort);

	}
}
var desord = [10, 1, 5, 4, 4, -3, 7, 8];
desord.quicksort;
desord;
