function toggleBar(id) {
    let e = document.getElementById(id);
    if (e.style.display == 'none') {
	e.style.display = 'block';
    } else {
	e.style.display = 'none';
    }
}
