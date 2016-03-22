var h = require('virtual-dom').h;

function h_wrapper(sel, props, children)
{
    var props_ = {};
    for (var i = 0; i < props.length; ++i)
    {
        props_[props[i][0]] = props[i][1];
    }

    var children_ = children.map(function (v, i, arr) { return v.slot1 });

    return h(sel, props_, children_);
}
