/*

let errorParams = new URLSearchParams(window.location.search);

let input_elements = [
    { default: "name",    error: "error-name",    element: document.getElementById('input-name')    },
    { default: "email",   error: "error-email",   element: document.getElementById('input-email')   },
    { default: "subject", error: "error-subject", element: document.getElementById('input-subject') },
    { default: "message", error: "error-message", element: document.getElementById('input-message') },

    // Referral data
    { default: "referral-src",   error: null, element: document.getElementById('input-referer') },
    { default: "referral-title", error: null, element: document.getElementById('page-title')    }
];

for (let i = 0; i < input_elements.length; i++) {
    if (errorParams.has(input_elements[i].default)) {
	try {
	    switch (input_elements[i].element.tagName) {
	    case 'HIDDEN': // Fall through
	    case 'INPUT':
		input_elements[i].element.setAttribute('value', errorParams.get(input_elements[i].default));
		break;
	    case 'H3': // Fall through
	    case 'TEXTAREA':
		input_elements[i].element.innerHTML = errorParams.get(input_elements[i].default);
		break;
	    case 'SELECT':
		input_elements[i].element.getElementsByTagName('option')[errorParams.get(input_elements[i].default)].selected = 'selected';
		break;
	    }
	} catch (err) {
	    // Passively report the error and move on.
	    console.log(err);
	}
    }

    if (errorParams.has(input_elements[i].error)) {
	input_elements[i].element.setAttribute('class', 'error-box');
    }
}
*/

function toggleBar(id) {
    let e = document.getElementById(id);
    if (e.style.display == 'none') {
	e.style.display = 'block';
    } else {
	e.style.display = 'none';
    }
}
