const urlParams = new URLSearchParams(window.location.search)
const isLive = urlParams.get('live') === 'true'

remark.macros.notLive = function() {
	return !isLive
}
