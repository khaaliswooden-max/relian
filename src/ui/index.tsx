import React from 'react';
import { createRoot } from 'react-dom/client';

const App = () => {
    return (
        <div className="p-4">
            <h1 className="text-2xl font-bold">Relian Frontend</h1>
            <p>Welcome to the Relian Legacy Refactoring Substrate UI.</p>
        </div>
    );
};

const container = document.getElementById('root');
if (container) {
    const root = createRoot(container);
    root.render(<App />);
}
