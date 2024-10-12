// Function to handle clicks on SVG elements
const handleSvgClick = event => {
    const target = event.target.closest('[id]')
    if (!target) {
        return
    }
    console.log('keycode', target.getAttribute('data-keycode'))
    target.classList.toggle('black')
    setTimeout(() => target.classList.toggle('black'), 100)
    if (target.classList.contains('led-key')) {
        const dataGroup = target.getAttribute('data-group')

        if (dataGroup) {
            if (!target.querySelector('.led').classList.contains('active')) {
                const groupElements = target.ownerDocument.querySelectorAll(`.led-key[data-group="${dataGroup}"]`)
                groupElements.forEach(element => {
                    element.querySelector('.led').classList.remove('active')
                })
                target.querySelector('.led').classList.toggle('active', true)
            }
        } else {
            target.querySelector('.led').classList.toggle('active')
        }
    }
}

// Function to set the active state of an element's LED child
const setLedActive = (elementId, isActive) => {
    const svgDoc = document.getElementById('svg-object').contentDocument
    const element = svgDoc.getElementById(elementId)
    if (element) {
        const led = element.querySelector('.led')
        if (led) {
            led.classList.toggle('active', isActive)
        }
    }
}

// Add event listener to the embedded SVG once it's loaded
document.getElementById('svg-object').addEventListener('load', () => {
    const svgDoc = document.getElementById('svg-object').contentDocument
    svgDoc.addEventListener('click', handleSvgClick)
})
