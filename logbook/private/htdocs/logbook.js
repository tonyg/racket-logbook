function install_callbacks(projectname, entrytype, entryname, tablename, controlcount) {
    var formname = 'table-' + tablename;
    var f = document.forms[formname];
    var xs = [];
    var ys = [];
    var logs = [];
    var img = document.getElementById('plot-' + tablename);

    var zerobasecontrol = 'zero-base-' + tablename;
    var zerobase_element = f[zerobasecontrol];

    function refresh_plot() {
	var xaxis = 0;
	var yaxes = [];
	var logaxes = [];
	for (var i = 0; i < ys.length; i++) {
	    if (xs[i].checked) xaxis = i;
	    if (ys[i].checked) yaxes.push(i);
	    if (logs[i].checked) logaxes.push(i);
	}
	if (yaxes.length) {
	    var logaxesstr = logaxes.join(',');
	    img.className = '';
	    img.src =
		'/log/' + projectname + '/' + entrytype + '/' + entryname + '/' + tablename +
		'/plot/' + xaxis + '/' + yaxes.join('/') + '?' + (+(new Date())) +
		'&logaxes=' + logaxesstr +
                (zerobase_element.checked ? '&y-min=0' : '');
	} else {
	    img.className = 'display-none';
	    img.src = '';
	}
    }

    zerobase_element.onclick = refresh_plot;

    for (var i = 0; i < controlcount; i++) {
	(function (i) {
	    var xcontrol = 'x-' + tablename + '-' + i;
	    var ycontrol = 'y-' + tablename + '-' + i;
	    var logcontrol = 'log-' + tablename + '-' + i;
	    var ex = f[xcontrol];
	    var ey = f[ycontrol];
	    var el = f[logcontrol];
	    xs.push(ex);
	    ys.push(ey);
	    logs.push(el);
	    ex.onclick = refresh_plot;
	    ey.onclick = refresh_plot;
	    el.onclick = refresh_plot;
	})(i);
    }
    refresh_plot();
}

function update_confirm_button_status() {
    var box = document.getElementById("confirm_checkbox");
    var button = document.getElementById("confirm_button");
    button.disabled = !box.checked;
}
