function install_callbacks(projectname, entrytype, entryname, tablename, controlcount) {
    var formname = 'table-' + tablename;
    var f = document.forms[formname];
    var xs = [];
    var ys = [];
    var img = document.getElementById('plot-' + tablename);

    function refresh_plot() {
	var xaxis = 0;
	var yaxes = [];
	for (var i = 0; i < ys.length; i++) {
	    if (xs[i].checked) xaxis = i;
	    if (ys[i].checked) yaxes.push(i);
	}
	if (yaxes.length) {
	    img.className = '';
	    img.src =
		'/log/' + projectname + '/' + entrytype + '/' + entryname + '/' + tablename +
		'/plot/' + xaxis + '/' + yaxes.join('/');
	} else {
	    img.className = 'display-none';
	    img.src = '';
	}
    }

    for (var i = 0; i < controlcount; i++) {
	(function (i) {
	    var xcontrol = 'x-' + tablename + '-' + i;
	    var ycontrol = 'y-' + tablename + '-' + i;
	    var ex = f[xcontrol];
	    var ey = f[ycontrol];
	    xs.push(ex);
	    ys.push(ey);
	    ex.onclick = refresh_plot;
	    ey.onclick = refresh_plot;
	})(i);
    }
    refresh_plot();
}

function update_confirm_button_status() {
    var box = document.getElementById("confirm_checkbox");
    var button = document.getElementById("confirm_button");
    button.disabled = !box.checked;
}
